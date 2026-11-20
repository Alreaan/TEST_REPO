// Loads ../HistoricalQuotes.csv into dbo.HistoricalQuotes (run sql/schema.sql first).
// Usage: npm run seed [-- path/to/file.csv]
const fs = require('node:fs');
const path = require('node:path');
const { sql, getPool, closePool } = require('../src/db');

const csvPath = process.argv[2] || path.join(__dirname, '..', '..', 'HistoricalQuotes.csv');
const money = (s) => Number(s.trim().replace('$', ''));

function toIsoDate(mmddyyyy) {
  const [m, d, y] = mmddyyyy.trim().split('/');
  return new Date(Date.UTC(+y, +m - 1, +d));
}

async function main() {
  const lines = fs.readFileSync(csvPath, 'utf8').trim().split(/\r?\n/).slice(1);

  const table = new sql.Table('dbo.HistoricalQuotes');
  table.create = false;
  table.columns.add('QuoteDate', sql.Date, { nullable: false });
  table.columns.add('OpenPrice', sql.Decimal(12, 4), { nullable: false });
  table.columns.add('HighPrice', sql.Decimal(12, 4), { nullable: false });
  table.columns.add('LowPrice', sql.Decimal(12, 4), { nullable: false });
  table.columns.add('ClosePrice', sql.Decimal(12, 4), { nullable: false });
  table.columns.add('Volume', sql.BigInt, { nullable: false });

  // CSV columns: Date, Close/Last, Volume, Open, High, Low
  for (const line of lines) {
    const [date, close, volume, open, high, low] = line.split(',');
    table.rows.add(toIsoDate(date), money(open), money(high), money(low), money(close), Number(volume.trim()));
  }

  const pool = await getPool();
  const tx = new sql.Transaction(pool);
  await tx.begin();
  try {
    await new sql.Request(tx).query('DELETE FROM dbo.HistoricalQuotes');
    await new sql.Request(tx).bulk(table);
    await tx.commit();
  } catch (err) {
    await tx.rollback();
    throw err;
  }
  console.log(`Loaded ${lines.length} rows from ${csvPath}`);
}

main()
  .catch((err) => {
    console.error(err);
    process.exitCode = 1;
  })
  .finally(closePool);
