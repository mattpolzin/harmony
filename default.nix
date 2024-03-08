{
  callPackage,
  fetchFromGitHub,
  git,
  buildIdris,
  lib,
  installShellFiles,
  makeWrapper,
  nodejs,
  stdenv,
}: let
  libraries = map (p: p.library {});

  nodeDependencies = (callPackage ./node2nix.nix {inherit nodejs;}).nodeDependencies;
  idrisAddsVersion = "0.4.0";
  elabUtilRev = "2fc2d188640ce6822b5e250db73b62f5a952ca4d";
  idrisJsonRev = "2e54a37ed3c35c2d12c8927c923ad253355812a8";
  idrisParserRev = "0fde36cf11c12a61edcfe09d585c5a60426bc706";

  idrisAddsSrc = fetchFromGitHub {
    owner = "mattpolzin";
    repo = "idris-adds";
    rev = "${idrisAddsVersion}";
    hash = "sha256-GzT31/8Mldn9feDKg/C5LfMYVWHV+FYZJPJo2li0fbE=";
  };
  elabUtilSrc = fetchFromGitHub {
    owner = "stefan-hoeck";
    repo = "idris2-elab-util";
    rev = "${elabUtilRev}";
    hash = "sha256-CYPrhB9y4CMk2Wiecpk+5isybcf3ZsbmaKdKOyo0JWk=";
  };
  idrisJsonSrc = fetchFromGitHub {
    owner = "stefan-hoeck";
    repo = "idris2-json";
    rev = "${idrisJsonRev}";
    hash = "sha256-+lwOdkovhOsvaSKH+jJY7uhr40JjXpUJ4ECR9qxZv14=";
  };
  idrisParserSrc = fetchFromGitHub {
    owner = "stefan-hoeck";
    repo = "idris2-parser";
    rev = "${idrisParserRev}";
    hash = "sha256-ShwVAUsobrwmuYszYld1RqlRUvnrACpyyqK2JKaIWYM=";
  };

  idrisAddsPkg = buildIdris {
    ipkgName = "idris-adds";
    version = idrisAddsVersion;
    src = idrisAddsSrc;
    idrisLibraries = [];
  };
  elabUtilPkg = buildIdris {
    ipkgName = "elab-util";
    version = elabUtilRev;
    src = elabUtilSrc;
    idrisLibraries = [];
  };
  idrisJsonPkg = buildIdris {
    ipkgName = "json";
    version = idrisJsonRev;
    src = idrisJsonSrc;
    idrisLibraries = libraries [elabUtilPkg idrisParserPkg idrisParserJsonPkg];
  };
  idrisParserPkg = buildIdris {
    ipkgName = "parser";
    version = idrisParserRev;
    src = idrisParserSrc;
    idrisLibraries = libraries [elabUtilPkg];
  };
  idrisParserJsonPkg = buildIdris rec {
    ipkgName = "parser-json";
    version = idrisParserRev;
    src = idrisParserSrc;
    sourceRoot = "${src.name}/json";
    idrisLibraries = libraries [idrisParserPkg elabUtilPkg];
  };

  harmonyPkg = buildIdris {
    version = "4.0.2";
    ipkgName = "harmony";
    src = ./.;

    idrisLibraries = libraries [idrisAddsPkg elabUtilPkg idrisParserPkg idrisParserJsonPkg idrisJsonPkg];
    nativeBuildInputs = [installShellFiles makeWrapper];
    buildInputs = [nodejs git];

    IDRIS2_DATA = "./support";

    postInstall = ''
      wrapProgram $out/bin/harmony \
        --prefix PATH : ${lib.makeBinPath [nodeDependencies git "$out"]} \
        --prefix NODE_PATH : ${nodeDependencies}/lib/node_modules

      installShellCompletion --cmd harmony \
        --bash <($out/bin/harmony --bash-completion-script) \
        --zsh <($out/bin/harmony --zsh-completion-script) \
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
