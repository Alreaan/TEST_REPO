# Quotes API (Node.js + Express + SQL Server)

A read-only REST API that serves the AAPL daily prices in `../HistoricalQuotes.csv` from a SQL Server database.

## Setup

1. **Install** (Node 18+): `npm install`
2. **Configure**: `cp .env.example .env` and set your server, database, user and password.
3. **Create the table**: run `sql/schema.sql` in SSMS, Azure Data Studio, or
   `sqlcmd -S localhost -U sa -P '<password>' -C -i sql/schema.sql`
4. **Load the data**: `npm run seed` (replaces the table contents with the CSV; pass another path with `npm run seed -- file.csv`)
5. **Run**: `npm start` (or `npm run dev` to restart on file changes)

No SQL Server yet? Start one in Docker:

```bash
docker run -d --name mssql -e ACCEPT_EULA=Y -e 'MSSQL_SA_PASSWORD=YourStrong!Passw0rd' \
  -p 1433:1433 mcr.microsoft.com/mssql/server:2022-latest
```

## Endpoints

| Method | Path | Description |
|---|---|---|
| GET | `/health` | API and database status (`503` if the DB is unreachable) |
| GET | `/api/quotes` | Paginated list. Query: `from`, `to` (YYYY-MM-DD), `page` (default 1), `pageSize` (1–500, default 50), `sort` (`asc`/`desc`, default `desc`) |
| GET | `/api/quotes/:date` | One trading day, e.g. `/api/quotes/2020-02-28` (`404` if none) |
| GET | `/api/quotes/summary` | Count, date range, min low, max high, average close and total volume. Query: `from`, `to` |

```bash
curl "http://localhost:3000/api/quotes?from=2019-01-01&to=2019-01-31&sort=asc&pageSize=5"
```

```json
{ "page": 1, "pageSize": 5, "total": 21, "totalPages": 5,
  "items": [{ "date": "2019-01-02", "open": 154.89, "high": 158.85, "low": 154.23, "close": 157.92, "volume": 35637070 }] }
```

Invalid parameters return `400` with `{ "error": "..." }`. Database errors return `500` without exposing details (they are logged on the server).

## Project layout

```
src/config.js            env-based settings
src/db.js                shared mssql connection pool
src/quotesRepository.js  parameterized SQL queries
src/validation.js        query/param validation
src/routes/quotes.js     /api/quotes routes
src/app.js               Express app, health check, error handling
src/server.js            entry point
sql/schema.sql           database + table
scripts/seed.js          CSV -> SQL Server bulk load
test/                    API tests (mocked repository, no DB needed): npm test
```

## Adding another table

Add a repository file with parameterized queries (`request.input(...)` — never concatenate user input into SQL), a router in `src/routes/`, and mount it in `src/app.js`.
