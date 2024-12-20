-- migrate:up
CREATE TABLE rooms (
    ruid TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    is_active BOOLEAN NOT NULL CHECK (is_active IN (0, 1)),
    created_at TEXT NOT NULL,
    players TEXT NOT NULL,
    owner TEXT NOT NULL,
    FOREIGN KEY (owner) REFERENCES users(uuid)
);

-- migrate:down
DROP TABLE IF EXISTS rooms;
