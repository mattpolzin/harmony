{
  fetchFromGitHub,
  git,
  buildIdris,
  lib,
  installShellFiles,
  makeBinaryWrapper,
  nodejs,
  buildNpmPackage,
  pandoc,
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
    src = lib.fileset.toSource {
      root = ./.; 
      fileset = lib.fileset.unions [
        ./package.json
        ./package-lock.json
        ./man/harmony.1
        ];
      };
    npmDepsHash = "sha256-N/pvX1PaSdDkAtB+3qirlXOwlRtjWQx/8246sbSzJUI=";
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
    pandoc
  ]
  ++ lib.optionals stdenv.isDarwin [ zsh ];
  buildInputs = [
    nodejs
    git
  ];

  IDRIS2_DATA = "./support";

  postBuild = ''
    make manpage

    installManPage man/harmony.1

    installShellCompletion --cmd harmony \
      --bash support/shell/bash-completions.sh \
      --zsh support/shell/zsh-completions.sh
  '';

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

  installCheckPhase = ''
    export harmony=$out/bin/harmony
    # The following tests are not run in Nix checks because they are currently
    # only designed to run with a github token in the environment:
    # - branch-command
    # - whoami-command
    INTERACTIVE="" except='branch-command whoami-command' make test
  '';

  meta = with lib; {
    description = "Harmony GitHub collaboration tool";
    homepage = "https://github.com/mattpolzin/harmony";
    license = licenses.mit;
    mainProgram = "harmony";
  };
}
