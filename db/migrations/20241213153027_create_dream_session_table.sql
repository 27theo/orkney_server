-- migrate:up
CREATE TABLE dream_session (
  id TEXT PRIMARY KEY,
  label TEXT NOT NULL,
  expires_at REAL NOT NULL,
  payload TEXT NOT NULL
);

-- migrate:down
DROP TABLE IF EXISTS dream_session;
