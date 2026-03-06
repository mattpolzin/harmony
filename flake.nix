{
  description = "Harmony GitHub collaboration tool";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    packageset.url = "github:mattpolzin/nix-idris2-packages";

    type-test-pkg.url = "github:mattpolzin/idris2-type-test";
    type-test-pkg.inputs.nixpkgs.follows = "nixpkgs";
    type-test-pkg.inputs.packageset.follows = "packageset";
  };

  outputs =
    {
      self,
      nixpkgs,
      packageset,
      type-test-pkg,
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
          buildIdris = packageset.buildIdris'.${system};
          type-test = type-test-pkg.packages.${system}.default;
          type-testApi = type-test-pkg.packages.${system}.type-testApi;
        in
        {
          harmony = pkgs.callPackage ./default.nix { inherit buildIdris type-test type-testApi; };

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
          type-tests = harmony.overrideAttrs { doCheck = true; };
        }
      );
      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (packageset.packages.${system}) idris2 idris2Lsp;
          type-test = type-test-pkg.packages.${system}.default;
        in
        {
          default = pkgs.mkShell {
            inputsFrom = [ self.packages.${system}.harmony.withSource ];
            packages = [
              idris2
              idris2Lsp
              type-test
            ];
          };
        }
      );
      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt);
    };

  nixConfig = {
    extra-substituters = [
      "https://gh-nix-idris2-packages.cachix.org"
      "https://gh-harmony.cachix.org"
    ];
  };
}
