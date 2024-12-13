# Lords of Orkney - Server

## Setup

### Dune developer preview

<https://preview.dune.build/>

`./scripts/install-dune.sh` should allow you to install the right dune version.

```
"Dune Developer Preview: build 2024-12-13T09:27:33Z, git revision
e4e98e11663a09d96605d44fd2d44f7403293d7e"
```

### Project creation

```
dune init proj server
cd server
dune pkg lock
dune exec server
```

### Adding a dependency

1. Add dependency to `dune-project`
2. `dune pkg lock`

## Managing the database

This project uses `dbmate`.

```
# To create the database from scratch
dbmate up
# To create a new migration
dbmate new <name_of_migration>
# To migrate and implement changes to schema
dbmate migrate
```
