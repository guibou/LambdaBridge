{
  description = "PyF";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.hls.url = "github:haskell/haskell-language-server";

  # Broken: see https://github.com/NixOS/nix/issues/5621
  nixConfig.extra-substituters = [
    "https://haskell-language-server.cachix.org"
  ];
  nixConfig.extra-trusted-public-keys = [
    "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
  ];

  outputs = { self, nixpkgs, flake-utils, hls }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        defaultPackage = pkgs.haskellPackages.callCabal2nix "LambdaBridge" ./. {};
        devShell = defaultPackage.env.overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs ++ (with pkgs; [ cabal-install haskell-language-server ]);
            });
      });
}
