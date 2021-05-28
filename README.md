# gerritbot-matrix

A matrix version of the [gerritbot][gerritbot].

## Usage

Create a `MATRIX_TOKEN` environement variable.
You can get it from element by visiting the account `Settings` page, `Help & About` panel, then click `Access Token`.

Then setup the configuration by editing the `gerritbot.dhall` file.

Run the bot with:

```ShellSession
$ gerritbot-matrix --gerrit-host review.opendev.org --gerrit-user tristanC --matrix-url https://matrix.org --config-file ./gerritbot.dhall
```

## Contribute

To work on this project you need a Haskell toolchain, for example on fedora:

```ShellSession
$ sudo dnf install -y ghc cabal-install && cabal update
```

Build and run the project:

```ShellSession
$ cabal run gerritbot-matrix -- --help
```

If you experience any difficulties, please don't hesistate to raise an issue.


[gerritbot]: https://opendev.org/opendev/gerritbot
