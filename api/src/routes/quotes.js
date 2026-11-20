const express = require('express');
const { HttpError, parseDate, parseIntInRange, parseDateRange } = require('../validation');

module.exports = function quotesRouter(repo) {
  const router = express.Router();

  // GET /api/quotes?from=YYYY-MM-DD&to=YYYY-MM-DD&page=1&pageSize=50&sort=desc
  router.get('/', async (req, res) => {
    const range = parseDateRange(req.query);
    const page = parseIntInRange(req.query.page, 'page', { def: 1, min: 1, max: 1_000_000 });
    const pageSize = parseIntInRange(req.query.pageSize, 'pageSize', { def: 50, min: 1, max: 500 });
    const sort = req.query.sort ?? 'desc';
    if (!['asc', 'desc'].includes(sort)) throw new HttpError(400, '"sort" must be "asc" or "desc"');

    const { total, items } = await repo.listQuotes({ ...range, page, pageSize, sort });
    res.json({ page, pageSize, total, totalPages: Math.ceil(total / pageSize), items });
  });

  // GET /api/quotes/summary?from=&to=
  router.get('/summary', async (req, res) => {
    res.json(await repo.getSummary(parseDateRange(req.query)));
  });

  // GET /api/quotes/2020-02-28
  router.get('/:date', async (req, res) => {
    const date = parseDate(req.params.date, 'date');
    const quote = await repo.getQuoteByDate(date);
    if (!quote) throw new HttpError(404, `No quote found for ${date}`);
    res.json(quote);
  });

  return router;
};
