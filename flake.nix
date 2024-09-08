{
  description = "Harmony GitHub collaboration tool";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    packageset.url = "github:mattpolzin/nix-idris2-packages";
  };

  # we use Idris2 from nixpkgs unstable because it will already be built and cached.
  # harmony should always build against the latest Idris2 release so this is a nice
  # default for the flake.
  outputs =
    {
      self,
      nixpkgs,
      packageset,
    }:
    let
      lib = nixpkgs.lib;
      forAllSystems = lib.genAttrs lib.systems.flakeExposed;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          idris2Packages = packageset.idris2Packages.${system};
          inherit (packageset.packages.${system}) buildIdris;
        in
        {
          harmony = pkgs.callPackage ./default.nix { inherit buildIdris idris2Packages; };

          default = self.packages.${system}.harmony;
        }
      );
      checks = forAllSystems (
        system:
        let
          harmony = self.packages.${system}.harmony;
        in
        {
          tests = harmony.overrideAttrs { doInstallCheck = true; };
        }
      );
      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (packageset.packages.${system}) idris2 idris2Lsp;
        in
        {
          default = pkgs.mkShell {
            inputsFrom = [ self.packages.${system}.harmony ];
            packages = [
              idris2
              idris2Lsp
            ];
          };
        }
      );
      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt-rfc-style);
    };

  nixConfig = {
    extra-substituters = [
      "https://gh-nix-idris2-packages.cachix.org"
      "https://gh-harmony.cachix.org"
    ];
  };
}
