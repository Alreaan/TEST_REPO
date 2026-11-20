const { test } = require('node:test');
const assert = require('node:assert/strict');
const request = require('supertest');
const { createApp } = require('../src/app');

const sample = { date: '2020-02-28', open: 257.26, high: 278.41, low: 256.37, close: 273.36, volume: 106721200 };

function fakeRepo(overrides = {}) {
  const calls = [];
  return {
    calls,
    ping: async () => {},
    listQuotes: async (args) => (calls.push(args), { total: 1, items: [sample] }),
    getQuoteByDate: async (d) => (d === sample.date ? sample : null),
    getSummary: async () => ({ days: 1 }),
    ...overrides,
  };
}

test('GET /api/quotes returns a paginated list with defaults', async () => {
  const repo = fakeRepo();
  const res = await request(createApp(repo)).get('/api/quotes');
  assert.equal(res.status, 200);
  assert.deepEqual(res.body, { page: 1, pageSize: 50, total: 1, totalPages: 1, items: [sample] });
  assert.deepEqual(repo.calls[0], { from: undefined, to: undefined, page: 1, pageSize: 50, sort: 'desc' });
});

test('GET /api/quotes passes filters through', async () => {
  const repo = fakeRepo();
  await request(createApp(repo)).get('/api/quotes?from=2019-01-01&to=2019-12-31&page=2&pageSize=10&sort=asc');
  assert.deepEqual(repo.calls[0], { from: '2019-01-01', to: '2019-12-31', page: 2, pageSize: 10, sort: 'asc' });
});

test('invalid query parameters return 400', async () => {
  const app = createApp(fakeRepo());
  for (const q of ['from=2020-13-01', 'from=2020-02-30', 'pageSize=0', 'pageSize=501', 'page=abc', 'sort=x', 'from=2020-02-01&to=2020-01-01']) {
    const res = await request(app).get(`/api/quotes?${q}`);
    assert.equal(res.status, 400, q);
    assert.ok(res.body.error);
  }
});

test('GET /api/quotes/:date returns one quote or 404', async () => {
  const app = createApp(fakeRepo());
  assert.deepEqual((await request(app).get('/api/quotes/2020-02-28')).body, sample);
  assert.equal((await request(app).get('/api/quotes/2020-02-27')).status, 404);
  assert.equal((await request(app).get('/api/quotes/not-a-date')).status, 400);
});

test('GET /api/quotes/summary', async () => {
  const res = await request(createApp(fakeRepo())).get('/api/quotes/summary');
  assert.equal(res.status, 200);
  assert.deepEqual(res.body, { days: 1 });
});

test('database errors become 500 without leaking details', async () => {
  const repo = fakeRepo({ listQuotes: async () => { throw new Error('Login failed for user sa'); } });
  const orig = console.error;
  console.error = () => {};
  const res = await request(createApp(repo)).get('/api/quotes');
  console.error = orig;
  assert.equal(res.status, 500);
  assert.deepEqual(res.body, { error: 'Internal server error' });
});

test('GET /health reports database status', async () => {
  assert.equal((await request(createApp(fakeRepo())).get('/health')).status, 200);
  const down = fakeRepo({ ping: async () => { throw new Error('down'); } });
  assert.equal((await request(createApp(down)).get('/health')).status, 503);
});
