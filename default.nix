{ callPackage
, fetchFromGitHub
, git
, idris2
, lib
, installShellFiles
, makeWrapper
, nodejs
, stdenv
}:
let 
  nodeDependencies = (callPackage ./node2nix.nix { inherit nodejs; }).nodeDependencies;
  idrisAddsVersion = "0.3.0";
  idrisAddsSrc = fetchFromGitHub {
    owner = "mattpolzin";
    repo = "idris-adds";
    rev = "${idrisAddsVersion}";
    hash = "sha256-OSu381nUNZqFJs4HzmMxGda60k7xsa1GulQq7kU/R2o=";
  };
  elabUtilRev = "2fc2d188640ce6822b5e250db73b62f5a952ca4d";
  elabUtilSrc = fetchFromGitHub {
    owner = "stefan-hoeck";
    repo = "idris2-elab-util";
    rev = "${elabUtilRev}";
    hash = "sha256-CYPrhB9y4CMk2Wiecpk+5isybcf3ZsbmaKdKOyo0JWk=";
  };
  idrisJsonRev = "2e54a37ed3c35c2d12c8927c923ad253355812a8";
  idrisJsonSrc = fetchFromGitHub {
    owner = "stefan-hoeck";
    repo = "idris2-json";
    rev = "${idrisJsonRev}";
    hash = "sha256-+lwOdkovhOsvaSKH+jJY7uhr40JjXpUJ4ECR9qxZv14=";
  };
  idrisParserRev = "0fde36cf11c12a61edcfe09d585c5a60426bc706";
  idrisParserSrc = fetchFromGitHub {
    owner = "stefan-hoeck";
    repo = "idris2-parser";
    rev = "${idrisParserRev}";
    hash = "sha256-ShwVAUsobrwmuYszYld1RqlRUvnrACpyyqK2JKaIWYM=";
  };
in
stdenv.mkDerivation (finalAttrs: {
  pname = "harmony";
  version = "3.0.0";

  nativeBuildInputs = [ idris2 installShellFiles makeWrapper ];
  buildInputs = [ nodejs git ];

  src = ./.;

  IDRIS_ADDS_SRC = "${idrisAddsSrc}";
  IDRIS_ELAB_UTIL_SRC = "${elabUtilSrc}";
  IDRIS_PARSER_SRC = "${idrisParserSrc}";
  IDRIS_JSON_SRC = "${idrisJsonSrc}";

  buildPhase = ''
    runHook preBuild

    ln -s ${nodeDependencies}/lib/node_modules ./node_modules
    export PATH="${nodeDependencies}/bin:$PATH"

    make build

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp harmony $out/bin/

    wrapProgram $out/bin/harmony \
      --prefix PATH : ${lib.makeBinPath [ nodeDependencies git "$out" ]} \
      --prefix NODE_PATH : ${nodeDependencies}/lib/node_modules

    installShellCompletion --cmd harmony \
      --bash <($out/bin/harmony --bash-completion-script) \
      --zsh <($out/bin/harmony --zsh-completion-script) \

    runHook postInstall
  '';

  meta = with lib; {
    description = "Harmony GitHub collaboration tool";
    homepage = "https://github.com/mattpolzin/harmony";
    license = licenses.mit;
    mainProgram = "harmony";
  };

})
