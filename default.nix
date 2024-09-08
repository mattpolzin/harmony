{
  callPackage,
  fetchFromGitHub,
  git,
  idris2Packages,
  buildIdris,
  lib,
  installShellFiles,
  makeBinaryWrapper,
  nodejs,
}:
let
  nodeDependencies = (callPackage ./node2nix.nix { inherit nodejs; }).nodeDependencies;
  idrisAddsVersion = "0.4.1";

  idrisAdds = buildIdris {
    ipkgName = "idris-adds";
    version = idrisAddsVersion;
    src = fetchFromGitHub {
      owner = "mattpolzin";
      repo = "idris-adds";
      rev = idrisAddsVersion;
      hash = "sha256-WEr6oRZ8+50G1qv7Kv62M4DRsgAa6x1BCODC1vDOQUY=";
    };
    idrisLibraries = [ ];
  };

  harmonyPkg = buildIdris {
    version = "5.0.0";
    ipkgName = "harmony";
    src = builtins.path {
      path = ./.;
      name = "harmony-pkg-src";
    };

    idrisLibraries =
      let
        ps = idris2Packages;
      in
      [
        idrisAdds
        ps.elab-util
        ps.parser
        ps.parser-json
        ps.json
      ];
    nativeBuildInputs = [
      installShellFiles
      makeBinaryWrapper
    ];
    buildInputs = [
      nodejs
      git
    ];

    IDRIS2_DATA = "./support";

    postInstall = ''
      wrapProgram $out/bin/harmony \
        --prefix PATH : ${
          lib.makeBinPath [
            nodeDependencies
            git
            "$out"
          ]
        } \
        --prefix NODE_PATH : ${nodeDependencies}/lib/node_modules
    '';

    postFixup = ''
      installShellCompletion --cmd harmony \
        --bash <($out/bin/harmony --bash-completion-script) \
        --zsh <($out/bin/harmony --zsh-completion-script) \
    '';

    installCheckPhase = ''
      export harmony=$out/bin/harmony
      INTERACTIVE="" make test
    '';

    meta = with lib; {
      description = "Harmony GitHub collaboration tool";
      homepage = "https://github.com/mattpolzin/harmony";
      license = licenses.mit;
      mainProgram = "harmony";
    };
  };
in
harmonyPkg.executable
