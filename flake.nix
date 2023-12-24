{
  description = "Harmony GitHub collaboration tool";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
  };

  outputs = { self, nixpkgs }:
  let 
    lib = nixpkgs.lib;
    forAllSystems = lib.genAttrs lib.systems.flakeExposed;
  in
  {
    packages = forAllSystems (system: 
      {
        harmony = with nixpkgs.legacyPackages.${system};
          callPackage ./default.nix {};

        default = self.packages.${system}.harmony;
      }
    );
  };
}
