{
  description = "Tagless Final Compiler";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    qbe-src = {
      url = "github:zack466/qbe";
    };
  };

  outputs = { self, nixpkgs, flake-utils, qbe-src }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # Access the default package from your fork's flake
        qbe = qbe-src.packages.${system}.default;
      in
      {
        devShells.default = pkgs.mkShell {
          name = "tagless-final-compiler";

          buildInputs = [
            qbe
          ];
        };
      }
    );
}
