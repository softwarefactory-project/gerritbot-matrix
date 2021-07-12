-- | Build the container with this command:
-- dhall text --file ./Containerfile.dhall | TMPDIR=/tmp podman build -t gerritbot-matrix -f - .
let base = "registry.fedoraproject.org/fedora:34"

let Containerfile =
      https://raw.githubusercontent.com/softwarefactory-project/dhall-containerfile/0.3.0/package.dhall sha256:03a6e298ff140d430cea8b387fad886ce9f5bee24622c7d1102115cc08ed9cf9

let clone-dep =
      \(url : Text) ->
      \(name : Text) ->
      \(rev : Text) ->
        let dest = "/build/${name}"

        in  Containerfile.run
              "Add dependency"
              [ "mkdir -p ${dest}"
              , "git clone ${url}/${name} ${dest}"
              , "cd ${dest}"
              , "git checkout ${rev}"
              ]

let copy =
      \(name : Text) ->
        Containerfile.copy [ "${name}/", "/build/gerritbot-matrix/${name}" ]

let image =
        Containerfile.from base
      # Containerfile.env (toMap { LANG = "C.UTF-8" })
      # Containerfile.workdir "/build/gerritbot-matrix"
      # Containerfile.run
          "Install haskell toolchain"
          [ "dnf update -y", "dnf install -y git ghc cabal-install" ]
      # Containerfile.run
          "Update cabal registry"
          [ "cabal v2-update", "mkdir -p /build/gerritbot-matrix" ]
      # clone-dep
          "https://softwarefactory-project.io/r/software-factory"
          "gerrit-haskell"
          "d2a39c1c56b561a2f3e9020dd7522774a23457ee"
      # clone-dep
          "https://softwarefactory-project.io/r/software-factory"
          "matrix-client-haskell"
          "bc64d7c8c4414f8282ef08721aebe4b427a807fc"
      # Containerfile.copy
          [ "cabal.project"
          , "gerritbot-matrix.cabal"
          , "LICENSE"
          , "/build/gerritbot-matrix"
          ]
      # Containerfile.run
          "Build dependencies"
          [ "cabal v2-build -v1 --dependencies-only all" ]
      # copy "app"
      # copy "src"
      # copy "test"
      # Containerfile.run
          "Build gerritbot"
          [ "cabal v2-install -v1 exe:gerritbot-matrix" ]
      # Containerfile.emptyLine
      # [ Containerfile.Statement.Comment "The final image" ]
      # Containerfile.from base
      # Containerfile.run
          "Install dependencies"
          [ "dnf update -y", "dnf install -y openssh-clients", "dnf clean all" ]
      # Containerfile.copyFrom
          "0"
          [ "/root/.cabal/bin/gerritbot-matrix", "/bin" ]
      # Containerfile.entrypoint [ "gerritbot-matrix" ]
      # Containerfile.label
          ( toMap
              { description = "A gerritbot for matrix"
              , maintainer = "tdecacqu@redhat.com"
              }
          )

in      ''
        # Copyright (C) 2021 Red Hat
        # SPDX-License-Identifier: Apache-2.0
        ''
    ++  Containerfile.render image
