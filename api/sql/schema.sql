-- Run in SSMS / Azure Data Studio / sqlcmd to create the database objects.
IF DB_ID('QuotesDb') IS NULL CREATE DATABASE QuotesDb;
GO
USE QuotesDb;
GO
IF OBJECT_ID('dbo.HistoricalQuotes', 'U') IS NULL
CREATE TABLE dbo.HistoricalQuotes (
    QuoteDate  DATE          NOT NULL PRIMARY KEY,
    OpenPrice  DECIMAL(12,4) NOT NULL,
    HighPrice  DECIMAL(12,4) NOT NULL,
    LowPrice   DECIMAL(12,4) NOT NULL,
    ClosePrice DECIMAL(12,4) NOT NULL,
    Volume     BIGINT        NOT NULL
);
GO
