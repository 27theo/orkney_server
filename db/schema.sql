CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(128) primary key);
CREATE TABLE users (
    uuid TEXT PRIMARY KEY,
    username TEXT NOT NULL UNIQUE,
    email TEXT NOT NULL UNIQUE,
    hashed_password TEXT NOT NULL
);
CREATE TABLE games (
    guid TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    is_active BOOLEAN NOT NULL CHECK (is_active IN (0, 1)),
    created_at TEXT NOT NULL,
    players TEXT NOT NULL,
    owner TEXT NOT NULL,
    FOREIGN KEY (owner) REFERENCES users(uuid)
);
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20241213142221'),
  ('20241217110752');
