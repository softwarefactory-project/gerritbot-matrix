# gerritbot-matrix

A matrix version of the [gerritbot][gerritbot].

## Usage

To use the bot, you need to create a `MATRIX_TOKEN` environment variable. See the [API_NOTES.md](./API_NOTES.md) token section.

Optionally, create a `MATRIX_IDENTITY_TOKEN` and `MATRIX_IDENTITY_PEPPER` environment variable to lookup author account.

Then setup the configuration by editing the `gerritbot.dhall` file.

Run the bot with:

```ShellSession
$ gerritbot-matrix --gerrit-host review.opendev.org --gerrit-user tristanC --homeserver-url https://matrix.org --config-file ./gerritbot.dhall
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

Build the container:

```ShellSession
$ TMPDIR=/tmp podman build -t quay.io/software-factory/gerritbot-matrix .
```

If you experience any difficulties, please don't hesistate to raise an issue.

[gerritbot]: https://opendev.org/opendev/gerritbot
