{
  description = "Harmony GitHub collaboration tool";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;

    idris.url = github:idris-lang/Idris2;
    idris.inputs.nixpkgs.follows = "nixpkgs";

    alejandra.url = github:kamadorueda/alejandra;
    alejandra.inputs.nixpkgs.follows = "nixpkgs";
  };

  # we grab Idris2 from nixpkgs unstable because it will already be built and cached.
  # we use the idris2 compiler repo to grab the buildIdris function that isn't available
  # in nixpkgs yet.
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
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
        buildIdris = idris.buildIdris.${system}.override {inherit (pkgs) idris2;};
      in {
        harmony = pkgs.callPackage ./default.nix {
          inherit buildIdris;
        };

        default = self.packages.${system}.harmony;
      }
    );
    formatter = forAllSystems (system: alejandra.packages.${system}.default);
  };
}
