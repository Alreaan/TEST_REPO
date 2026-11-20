const express = require('express');
const quotesRouter = require('./routes/quotes');

/** Builds the Express app. `repo` is injectable so tests can run without a database. */
function createApp(repo = require('./quotesRepository')) {
  const app = express();
  app.disable('x-powered-by');
  app.use(express.json());

  app.get('/health', async (req, res) => {
    try {
      await repo.ping();
      res.json({ status: 'ok', database: 'up' });
    } catch {
      res.status(503).json({ status: 'degraded', database: 'down' });
    }
  });

  app.use('/api/quotes', quotesRouter(repo));

  app.use((req, res) => res.status(404).json({ error: 'Not found' }));

  // Express 5 forwards rejected promises from async handlers here.
  // eslint-disable-next-line no-unused-vars
  app.use((err, req, res, next) => {
    const status = err.status || 500;
    if (status >= 500) console.error(err);
    res.status(status).json({ error: status >= 500 ? 'Internal server error' : err.message });
  });

  return app;
}

module.exports = { createApp };
