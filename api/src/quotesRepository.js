// Data-access layer: every query is parameterized to prevent SQL injection.
const { sql, getPool } = require('./db');

// CONVERT(..., 23) gives 'YYYY-MM-DD'; daily volumes fit safely in a JS number.
const COLUMNS = 'CONVERT(char(10), QuoteDate, 23) AS date, OpenPrice AS [open], HighPrice AS high, LowPrice AS low, ClosePrice AS [close], CAST(Volume AS FLOAT) AS volume';

function addDateRange(request, where, { from, to }) {
  if (from) {
    request.input('from', sql.Date, from);
    where.push('QuoteDate >= @from');
  }
  if (to) {
    request.input('to', sql.Date, to);
    where.push('QuoteDate <= @to');
  }
}

async function listQuotes({ from, to, page, pageSize, sort }) {
  const pool = await getPool();
  const request = pool.request();
  const where = [];
  addDateRange(request, where, { from, to });
  const whereSql = where.length ? `WHERE ${where.join(' AND ')}` : '';
  const order = sort === 'asc' ? 'ASC' : 'DESC'; // whitelisted, never user text

  request.input('offset', sql.Int, (page - 1) * pageSize);
  request.input('pageSize', sql.Int, pageSize);

  const result = await request.query(`
    SELECT COUNT(*) AS total FROM dbo.HistoricalQuotes ${whereSql};
    SELECT ${COLUMNS}
    FROM dbo.HistoricalQuotes ${whereSql}
    ORDER BY QuoteDate ${order}
    OFFSET @offset ROWS FETCH NEXT @pageSize ROWS ONLY;
  `);

  return { total: result.recordsets[0][0].total, items: result.recordsets[1] };
}

async function getQuoteByDate(date) {
  const pool = await getPool();
  const result = await pool
    .request()
    .input('date', sql.Date, date)
    .query(`SELECT ${COLUMNS} FROM dbo.HistoricalQuotes WHERE QuoteDate = @date`);
  return result.recordset[0] || null;
}

async function getSummary({ from, to }) {
  const pool = await getPool();
  const request = pool.request();
  const where = [];
  addDateRange(request, where, { from, to });
  const whereSql = where.length ? `WHERE ${where.join(' AND ')}` : '';
  const result = await request.query(`
    SELECT COUNT(*) AS days,
           CONVERT(char(10), MIN(QuoteDate), 23) AS firstDate, CONVERT(char(10), MAX(QuoteDate), 23) AS lastDate,
           MIN(LowPrice) AS minLow, MAX(HighPrice) AS maxHigh,
           AVG(ClosePrice) AS avgClose, CAST(SUM(Volume) AS FLOAT) AS totalVolume
    FROM dbo.HistoricalQuotes ${whereSql}
  `);
  return result.recordset[0];
}

async function ping() {
  const pool = await getPool();
  await pool.request().query('SELECT 1 AS ok');
}

module.exports = { listQuotes, getQuoteByDate, getSummary, ping };
