{
  description = "A c compiler written in awk";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
        with pkgs; {
          devShells.default = pkgsCross.riscv64-musl.mkShell {
            buildInputs = [
              gawk
              nodejs
            ];
          };
          devShells.prettierInstall = pkgs.mkShell {
            buildInputs = [
              nodePackages.npm
              node-gyp
            ];
          };
        }
    );
}
