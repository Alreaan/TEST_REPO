const config = require('./config');
const { createApp } = require('./app');
const { closePool } = require('./db');

const server = createApp().listen(config.port, () => {
  console.log(`API listening on http://localhost:${config.port}`);
});

async function shutdown() {
  server.close();
  await closePool().catch(() => {});
  process.exit(0);
}
process.on('SIGINT', shutdown);
process.on('SIGTERM', shutdown);
