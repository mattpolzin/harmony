{
  fetchFromGitHub,
  git,
  buildIdris,
  lib,
  installShellFiles,
  makeBinaryWrapper,
  nodejs,
  buildNpmPackage,
  stdenv,
  zsh,
}:
let
  idrisAddsVersion = "0.5.0";

  idrisAdds = buildIdris {
    ipkgName = "idris-adds";
    src = fetchFromGitHub {
      owner = "mattpolzin";
      repo = "idris-adds";
      rev = idrisAddsVersion;
      hash = "sha256-Ze95Bt89YuizWD8lSmkpcQST774mPYCEm/UnCBhE5I0=";
    };
  };

  nodeDependencies = buildNpmPackage {
    name = "harmony-npm-deps";
    src = ./.;
    npmDepsHash = "sha256-i2lDvXenlqSq8CmmfXehxKeDsXKSo9Eqw8BXm9aCAHc=";
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
  ]
  ++ lib.optionals stdenv.isDarwin [ zsh ];
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
