{
  description = "Harmony GitHub collaboration tool";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;

    idris.url = github:mattpolzin/Idris2/fix-install-output-buildIdris;
    idris.inputs.nixpkgs.follows = "nixpkgs";

    alejandra.url = github:kamadorueda/alejandra;
    alejandra.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    alejandra,
    idris,
  }: let
    lib = nixpkgs.lib;
    forAllSystems = lib.genAttrs lib.systems.flakeExposed;
  in {
    packages = forAllSystems (
      system: {
        harmony = with nixpkgs.legacyPackages.${system};
          callPackage ./default.nix {
            idris2 = idris.packages.${system}.idris2;
            buildIdris = idris.buildIdris.${system};
          };

        default = self.packages.${system}.harmony;
      }
    );
    formatter = forAllSystems (system: alejandra.packages.${system}.default);
  };
}
