-- migrate:up
ALTER TABLE games
ADD state text;

-- migrate:down
ALTER TABLE games
DROP COLUMN state;
