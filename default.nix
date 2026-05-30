{
  fetchFromGitHub,
  git,
  buildIdris',
  lib,
  installShellFiles,
  makeBinaryWrapper,
  nodejs,
  buildNpmPackage,
  pandoc,
  stdenv,
  zsh,
  type-test,
  type-testApi,
}:
let
  idrisAddsVersion = "0.5.0";

  idrisAdds = buildIdris' {
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
    npmDepsHash = "sha256-DKWp1AkI/zqbrrcOXT4O8T9PGugr6+vcH5z+3kGqss4=";
    dontNpmBuild = true;
    dontBuild = true;

    postInstall = ''
      rm -rf $out/bin
      mv $out/lib/node_modules/@mattpolzin/harmony/node_modules $out/node_modules
      rm -rf $out/lib
    '';
  };

  harmony-test = import ./test { inherit buildIdris'; };

  harmony = buildIdris' {
    ipkgName = "harmony";
    src = builtins.path {
      path = ./.;
      name = "harmony-pkg-src";
    };

    extraIdrisLibraries = [
      idrisAdds
      type-testApi
    ];

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
  };
in
harmony.overrideAttrs {
  postBuild = ''
    make manpage

    installManPage man/harmony.1

    installShellCompletion --cmd harmony \
      --bash support/shell/bash-completions.sh \
      --zsh support/shell/zsh-completions.sh
  '';

  nativeCheckInputs = [ type-test ];
  checkPhase = ''
    find src -name 'Test.idr' | \
      xargs type-test --find-ipkg
    mkdir -p $out
    # ^ this means we can run the checkPhase without the build or install phases
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

  nativeInstallCheckInputs = [ harmony-test ];
  installCheckPhase = ''
    export harmony=$out/bin/harmony
    export idris2=idris2
    # The following tests are not run in Nix checks because they are currently
    # only designed to run with a github token in the environment:
    # - branch-command
    # - whoami-command
    cd test
    harmony-test runtests --except 'branch-command whoami-command'
  '';

  meta = with lib; {
    description = "Harmony GitHub collaboration tool";
    homepage = "https://github.com/mattpolzin/harmony";
    license = licenses.mit;
    mainProgram = "harmony";
  };
}
