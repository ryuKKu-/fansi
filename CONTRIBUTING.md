# Contributing guidelines

## Set up

You need the .NET SDK version in [`global.json`](global.json). The repository uses
[Paket](https://fsprojects.github.io/Paket/) for packages and
[Fantomas](https://fsprojects.github.io/fantomas/) for formatting. Both come as local tools.

```shell
dotnet tool restore
dotnet paket restore
dotnet build
```

The build has no warnings, and it should stay that way. CI builds with `-warnaserror`.

## Run the tests

```shell
dotnet test tests/Fansi.Tests
```

The tests do not need a real terminal. Layout, painting and the input parser are tested on plain data.
`tests/Fansi.Tests` has one folder for each folder in `src/Fansi`, so a new test goes next to the code it covers.

## Code style

Format the code before you commit. CI fails if a file needs formatting.

```shell
dotnet fantomas src tests samples
```

Write comments the way you would explain the code to a colleague. Use short sentences and plain words.

## Commits

Commit messages are one line, in the
[conventional commits](https://www.conventionalcommits.org) style.

## Pull requests

1. Branch from `master`.
2. Make sure the build, the tests and the formatting check pass.
3. Open a pull request with a short description of what changed and why.

CI runs on Linux and Windows. A pull request merges when both pass.

## Releases

You do not release by hand. When changes reach `master`, the release tool opens a release pull request with the
new version and the changelog. Merging that pull request publishes the package.
