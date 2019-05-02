CREATE TABLE IF NOT EXISTS trove_article (
    id              SERIAL PRIMARY KEY, -- auto-incremented numerical id
    art_id          INTEGER, -- Trove's id for the article
    heading         VARCHAR(255), -- the title of the article
    category        VARCHAR(255), -- Trove's categorisation (not very useful)
    newspaper_id    INTEGER, -- the id of the newspaper (refers to trove_newspaper)
    date            DATE, -- date the article was published
    text            TEXT, -- the text of the article
    tokens          VARCHAR(255)[] -- tokens of the article (stopwords removed)
);

CREATE TABLE IF NOT EXISTS trove_newspaper (
    news_id         INTEGER CONSTRAINT newspaper_id PRIMARY KEY, -- Trove's id for the newspaper
    title           VARCHAR(255), -- The title of the newspaper
    state           VARCHAR(20), -- The state the newspaper was published in (sometimes 'National' or 'International')
    issn            CHAR(8), -- The ISSN of the newspaper
    troveUrl        VARCHAR(50), -- The url where the newspaper's metadata can be found/the portal to its articles
    startDate       DATE, -- The first date the newspaper was published (as far as Trove knows)
    endDate         DATE, -- The date of the final issue (as far as Trove knows)
    location        VARCHAR(50), -- The location (i.e. town or city) where the newspaper was published
    geoname         INTEGER, -- The geoname for that location
    lat             NUMERIC(10), -- The latitidue of the location (from geonames)
    lon             NUMERIC(10) -- The longitude of the location (from geonames)
);