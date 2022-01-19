{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hs = pkgs.haskellPackages;
      in
      rec {
        packages.snake = hs.callCabal2nix "snake" ./. { };
        defaultPackage = packages.snake;

        apps.snake = flake-utils.lib.mkApp { drv = packages.snake; };
        defaultApp = apps.snake;

        devShell = packages.snake.env.overrideAttrs
          (super: {
            buildInputs = with hs; super.buildInputs ++ [
              cabal-install
              fourmolu
              haskell-language-server
              hlint
            ];
          });
      });
}
