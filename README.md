# Orkney

## Setup

### Dune developer preview

```
"Dune Developer Preview: build 2024-12-13T09:27:33Z, git revision
e4e98e11663a09d96605d44fd2d44f7403293d7e"
```

### Project creation

```
dune init proj orkney
cd orkney
dune pkg lock
dune exec orkney
```

### Adding a dependency

1. Add dependency to `dune-project`
2. `dune pkg lock`
