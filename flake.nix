# Build the container image using:
#   nix build -L .#containerImage
#   TMPDIR=/tmp/podman podman load < result
{
  description = "The gerritbot-matrix application";

  nixConfig.bash-prompt = "[nix]λ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    gerrit.url = "github:softwarefactory-project/gerrit-haskell";
    gerrit.flake = false;
    matrix-client.url = "github:softwarefactory-project/matrix-client-haskell";
    matrix-client.flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, gerrit, matrix-client }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        # config = { allowBroken = true; };
        config = { };

        compilerVersion = "8104";
        compiler = "ghc" + compilerVersion;
        overlays = [
          (final: prev: {
            haskell-language-server = prev.haskell-language-server.override {
              supportedGhcVersions = [ compilerVersion ];
            };

            myHaskellPackages = prev.haskell.packages.${compiler}.override {
              overrides = hpFinal: hpPrev: {
                relude = prev.haskell.lib.overrideCabal hpPrev.relude {
                  version = "1.0.0.1";
                  sha256 =
                    "0cw9a1gfvias4hr36ywdizhysnzbzxy20fb3jwmqmgjy40lzxp2g";
                };
                gerrit = hpPrev.callCabal2nix "gerrit" gerrit { };
                matrix-client =
                  hpPrev.callCabal2nix "matrix-client" matrix-client { };
                gerritbot-matrix =
                  hpPrev.callCabal2nix "gerritbot-matrix" ./. { };
              };
            };

          })
        ];

        pkgs = import nixpkgs { inherit config overlays system; };

      in rec {
        defaultPackage = packages.gerritbot-matrix;
        defaultApp = apps.gerritbot-matrix;
        defaultExe = pkgs.haskell.lib.justStaticExecutables defaultPackage;
        defaultContainerImage = pkgs.dockerTools.buildLayeredImage {
          name = "gerritbot-matrix";
          contents = [ defaultExe pkgs.openssh pkgs.cacert ];
          extraCommands = "echo root:x:0:0:root:/root:/bin/bash > etc/passwd";
          config = {
            Entrypoint = [ "gerritbot-matrix" ];
            Env = [ "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt" "HOME=/root" ];
          };
        };

        packages = with pkgs.myHaskellPackages; {
          inherit gerritbot-matrix;
          containerImage = defaultContainerImage;
        };

        apps.gerritbot-matrix =
          flake-utils.lib.mkApp { drv = packages.gerritbot-matrix; };

        devShell = pkgs.myHaskellPackages.shellFor {
          packages = p: [ p.gerritbot-matrix ];

          buildInputs = with pkgs.myHaskellPackages; [
            cabal-install
            hlint
            pkgs.haskell-language-server
          ];

          withHoogle = false;
        };
      });
}
