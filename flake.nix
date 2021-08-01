# Build the container image using:
#   nix build -L .#containerImage
#   TMPDIR=/tmp/podman podman load < result
{
  description = "The gerritbot-matrix application";

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
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        packageName = "gerritbot-matrix";
      in rec {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self {
            relude = pkgs.haskell.lib.overrideCabal haskellPackages.relude {
              version = "1.0.0.1";
              sha256 = "0cw9a1gfvias4hr36ywdizhysnzbzxy20fb3jwmqmgjy40lzxp2g";
            };

            gerrit = haskellPackages.callCabal2nix "gerrit" gerrit { };
            matrix-client =
              haskellPackages.callCabal2nix "matrix-client" matrix-client { };
          };

        packages.exe =
          pkgs.haskell.lib.justStaticExecutables (packages.${packageName});

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };

        packages.containerImage = pkgs.dockerTools.buildLayeredImage {
          name = "gerritbot-matrix";
          contents = [ packages.exe pkgs.openssh pkgs.curl pkgs.cacert ];
          extraCommands = "echo root:x:0:0:root:/root:/bin/bash > etc/passwd";
          config = {
            Entrypoint = [ "gerritbot-matrix" ];
            Env = [ "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt" "HOME=/root" ];
          };
        };

      });
}
