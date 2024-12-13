CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(128) primary key);
CREATE TABLE users (
    uuid TEXT PRIMARY KEY,
    username TEXT NOT NULL UNIQUE,
    email TEXT NOT NULL UNIQUE,
    hashed_password TEXT NOT NULL
);
CREATE TABLE dream_session (
  id TEXT PRIMARY KEY,
  label TEXT NOT NULL,
  expires_at REAL NOT NULL,
  payload TEXT NOT NULL
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20241213142221'),
  ('20241213153027');
