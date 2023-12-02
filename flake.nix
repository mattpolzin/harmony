{
  description = "Harmony GitHub collaboration tool";

  inputs = {
    nixpkgs.url = "https://github.com/nixos/nixpkgs";
  };

  outputs = { self, nixpkgs }:
  let 
    lib = nixpkgs.lib;
    forAllSystems = lib.genAttrs lib.systems.flakeExposed;
  in
  {
    packages = forAllSystems (system: 
      {
        harmony = with nixpkgs.legacyPackages.${system}.default;
          callPackage ./default.nix {};

        default = self.packages.${system}.harmony;
      }
    );
  };
}
