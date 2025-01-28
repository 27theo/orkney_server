# Lords of Orkney - Server

## Setup

### Dependencies

#### The dune developer preview

<https://preview.dune.build/>

`./scripts/install-dune.sh` has the correct version hard-coded, so running the
script should install the version that the maintainers are using to maintain
the project.

```bash
$ dune --version
"Dune Developer Preview: build 2024-12-13T09:27:33Z, git revision
e4e98e11663a09d96605d44fd2d44f7403293d7e"
```

#### Dbmate

This project uses `dbmate` to manage the database. Installation instructions
can be found [in the GitHub repo](https://github.com/amacneil/dbmate).

### Initial setup

Copy the `.env.template` into a new `.env` file. The values in the .env file
will be used by the server. Fill in the missing value (SECRET_KEY).

```bash
cp .env.template .env
vim .env
```

To create the database from the schema at `./db/schema.sql`:

```bash
dbmate up
```

The first time you run `dune exec server`, you may be instructed to install
system dependencies. Run it, and do so.

```bash
dune exec server
```

## Development

### Running the server

```bash
dune exec server
```

### Adding a dependency

1. Add dependency to `dune-project`
2. Run `dune pkg lock`

### Database management

```bash
# To create the database from scratch
dbmate up
# To create a new migration
dbmate new [name_of_migration]
# To migrate and implement changes to schema
dbmate migrate
```
