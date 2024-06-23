{
  description = "Harmony GitHub collaboration tool";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    alejandra.url = "github:kamadorueda/alejandra";
    alejandra.inputs.nixpkgs.follows = "nixpkgs";

    idris2.url = "github:mattpolzin/Idris2/nix-target-only-executable";
  };

  # we use Idris2 from nixpkgs unstable because it will already be built and cached.
  # harmony should always build against the latest Idris2 release so this is a nice
  # default for the flake.
  outputs = {
    self,
    nixpkgs,
    alejandra,
    idris2
  }: let
    lib = nixpkgs.lib;
    forAllSystems = lib.genAttrs lib.systems.flakeExposed;
  in {
    packages = forAllSystems (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        harmony = pkgs.callPackage ./default.nix { buildIdris = idris2.buildIdris.${system}; };

        default = self.packages.${system}.harmony;
      }
    );
    devShells = forAllSystems (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        default = pkgs.mkShell {
          inputsFrom = [self.packages.${system}.harmony];
          packages = [
            pkgs.idris2Packages.idris2Lsp
          ];
        };
      }
    );
    formatter = forAllSystems (system: alejandra.packages.${system}.default);
  };
}
