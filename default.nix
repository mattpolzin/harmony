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
    src = ./.;
    npmDepsHash = "sha256-eT2dv5ixalqD7mbabN/sIg2sUNj0z0aj1K3iaObAmgs=";
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

  postFixup = ''
    installShellCompletion --cmd harmony \
      --bash <($out/bin/harmony --bash-completion-script) \
      --zsh <($out/bin/harmony --zsh-completion-script)
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
