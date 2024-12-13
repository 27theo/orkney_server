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
