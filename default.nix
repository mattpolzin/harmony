{
  callPackage,
  fetchFromGitHub,
  git,
  idris2Packages,
  lib,
  installShellFiles,
  makeWrapper,
  nodejs,
}:
let
  inherit (idris2Packages) buildIdris;

  nodeDependencies = (callPackage ./node2nix.nix { inherit nodejs; }).nodeDependencies;
  idrisAddsVersion = "0.4.1";
  elabUtilRev = "2fc2d188640ce6822b5e250db73b62f5a952ca4d";
  idrisJsonRev = "2e54a37ed3c35c2d12c8927c923ad253355812a8";
  idrisParserRev = "0fde36cf11c12a61edcfe09d585c5a60426bc706";

  idrisAddsSrc = fetchFromGitHub {
    owner = "mattpolzin";
    repo = "idris-adds";
    rev = "${idrisAddsVersion}";
    hash = "sha256-WEr6oRZ8+50G1qv7Kv62M4DRsgAa6x1BCODC1vDOQUY=";
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

  idrisAdds = buildIdris {
    ipkgName = "idris-adds";
    version = idrisAddsVersion;
    src = idrisAddsSrc;
    idrisLibraries = [ ];
  };
  elabUtil = buildIdris {
    ipkgName = "elab-util";
    version = elabUtilRev;
    src = elabUtilSrc;
    idrisLibraries = [ ];
  };
  idrisJson = buildIdris {
    ipkgName = "json";
    version = idrisJsonRev;
    src = idrisJsonSrc;
    idrisLibraries = [
      elabUtil
      idrisParser
      idrisParserJson
    ];
  };
  idrisParser = buildIdris {
    ipkgName = "parser";
    version = idrisParserRev;
    src = idrisParserSrc;
    idrisLibraries = [ elabUtil ];
  };
  idrisParserJson = buildIdris rec {
    ipkgName = "parser-json";
    version = idrisParserRev;
    src = idrisParserSrc;
    sourceRoot = "${src.name}/json";
    idrisLibraries = [
      idrisParser
      elabUtil
    ];
  };

  harmonyPkg = buildIdris {
    version = "5.0.0";
    ipkgName = "harmony";
    src = ./.;

    idrisLibraries = [
      idrisAdds
      elabUtil
      idrisJson
      idrisParserJson
    ];
    nativeBuildInputs = [
      installShellFiles
      makeWrapper
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
