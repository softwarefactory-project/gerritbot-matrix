# Copyright (C) 2021 Red Hat
# SPDX-License-Identifier: Apache-2.0
FROM registry.fedoraproject.org/fedora:33
ENV LANG C.UTF-8
WORKDIR /build/gerritbot-matrix
# Install haskell toolchain
RUN dnf update -y && dnf install -y git ghc cabal-install

# Update cabal registry
RUN cabal v2-update && mkdir -p /build/gerritbot-matrix

# Add dependency
RUN mkdir -p /build/gerrit-haskell && git clone https://softwarefactory-project.io/r/software-factory/gerrit-haskell /build/gerrit-haskell && cd /build/gerrit-haskell && git checkout d2a39c1c56b561a2f3e9020dd7522774a23457ee

# Add dependency
RUN mkdir -p /build/matrix-client-haskell && git clone https://softwarefactory-project.io/r/software-factory/matrix-client-haskell /build/matrix-client-haskell && cd /build/matrix-client-haskell && git checkout 4c5b8c6719085edd97791306d5d7c13be3b1587b

COPY ["cabal.project", "gerritbot-matrix.cabal", "LICENSE", "/build/gerritbot-matrix"]
# Build dependencies
RUN cabal v2-build -v1 --dependencies-only all

COPY ["app/", "/build/gerritbot-matrix/app"]
COPY ["src/", "/build/gerritbot-matrix/src"]
COPY ["test/", "/build/gerritbot-matrix/test"]
# Build gerritbot
RUN cabal v2-install -v1 exe:gerritbot-matrix


# The final image
FROM registry.fedoraproject.org/fedora:33
# Install dependencies
RUN dnf update -y && dnf install -y openssh-clients && dnf clean all

COPY --from=0 ["/root/.cabal/bin/gerritbot-matrix", "/bin"]
ENTRYPOINT ["gerritbot-matrix"]
LABEL description="A gerritbot for matrix"
LABEL maintainer="tdecacqu@redhat.com"
