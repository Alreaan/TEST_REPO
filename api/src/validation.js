const DATE_RE = /^\d{4}-\d{2}-\d{2}$/;

class HttpError extends Error {
  constructor(status, message) {
    super(message);
    this.status = status;
  }
}

function parseDate(value, name) {
  if (value === undefined || value === '') return undefined;
  const d = new Date(`${value}T00:00:00Z`);
  if (!DATE_RE.test(value) || Number.isNaN(d.getTime()) || d.toISOString().slice(0, 10) !== value) {
    throw new HttpError(400, `"${name}" must be a valid date in YYYY-MM-DD format`);
  }
  return value;
}

function parseIntInRange(value, name, { def, min, max }) {
  if (value === undefined || value === '') return def;
  const n = Number(value);
  if (!Number.isInteger(n) || n < min || n > max) {
    throw new HttpError(400, `"${name}" must be an integer between ${min} and ${max}`);
  }
  return n;
}

function parseDateRange(query) {
  const from = parseDate(query.from, 'from');
  const to = parseDate(query.to, 'to');
  if (from && to && from > to) throw new HttpError(400, '"from" must not be after "to"');
  return { from, to };
}

module.exports = { HttpError, parseDate, parseIntInRange, parseDateRange };
