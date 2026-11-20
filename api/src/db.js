const sql = require('mssql');
const config = require('./config');

let poolPromise;

/** Returns a shared connection pool, created on first use. */
function getPool() {
  if (!poolPromise) {
    poolPromise = new sql.ConnectionPool(config.db)
      .connect()
      .catch((err) => {
        poolPromise = undefined; // allow retry on next request
        throw err;
      });
  }
  return poolPromise;
}

async function closePool() {
  if (poolPromise) {
    const pool = await poolPromise;
    poolPromise = undefined;
    await pool.close();
  }
}

module.exports = { sql, getPool, closePool };
