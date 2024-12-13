-- migrate:up
CREATE TABLE users (
    uuid TEXT PRIMARY KEY,
    username TEXT NOT NULL UNIQUE,
    email TEXT NOT NULL UNIQUE,
    hashed_password TEXT NOT NULL
);

-- migrate:down
DROP TABLE IF EXISTS users;
