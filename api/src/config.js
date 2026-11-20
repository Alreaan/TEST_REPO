require('dotenv').config();

const bool = (v, def) => (v === undefined ? def : String(v).toLowerCase() === 'true');

module.exports = {
  port: Number(process.env.PORT) || 3000,
  db: {
    server: process.env.DB_SERVER || 'localhost',
    port: Number(process.env.DB_PORT) || 1433,
    database: process.env.DB_NAME || 'QuotesDb',
    user: process.env.DB_USER,
    password: process.env.DB_PASSWORD,
    pool: { max: 10, min: 0, idleTimeoutMillis: 30000 },
    options: {
      encrypt: bool(process.env.DB_ENCRYPT, false),
      trustServerCertificate: bool(process.env.DB_TRUST_SERVER_CERTIFICATE, true),
    },
  },
};
