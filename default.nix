{
  fetchFromGitHub,
  git,
  buildIdris,
  lib,
  installShellFiles,
  makeBinaryWrapper,
  nodejs,
  buildNpmPackage,
}:
let
  idrisAddsVersion = "0.4.1";

  idrisAdds = buildIdris {
    ipkgName = "idris-adds";
    src = fetchFromGitHub {
      owner = "mattpolzin";
      repo = "idris-adds";
      rev = idrisAddsVersion;
      hash = "sha256-WEr6oRZ8+50G1qv7Kv62M4DRsgAa6x1BCODC1vDOQUY=";
    };
  };

  nodeDependencies  = buildNpmPackage {
    name = "harmony-npm-deps";
    src = ./.;
    npmDepsHash = "sha256-GRnI5EBmGnkXIsY3SK9Ma0uVias03GvqpMqdZOqfqqQ=";
    dontNpmBuild = true;
    dontBuild = true;

    postInstall = ''
      rm -rf $out/bin
      mv $out/lib/node_modules/@mattpolzin/harmony/node_modules $out/node_modules
      rm -rf $out/lib
    '';
  };
in
buildIdris {
  ipkgName = "harmony";
  src = builtins.path {
    path = ./.;
    name = "harmony-pkg-src";
  };

  extraIdrisLibraries = [ idrisAdds ];

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
          git
          "$out"
        ]
      } \
      --prefix NODE_PATH : ${nodeDependencies}/node_modules
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
}
