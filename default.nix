{ stdenv, callPackage, fetchFromGitHub, idris2 }:
let 
  nodeDependencies = (callPackage ./node2nix.nix {}).nodeDependencies;
  idrisAddsVersion = "0.3.0";
  idrisAddsSrc = fetchFromGitHub {
    owner = "mattpolzin";
    repo = "idris-adds";
    rev = "${idrisAddsVersion}";
    hash = "sha256-OSu381nUNZqFJs4HzmMxGda60k7xsa1GulQq7kU/R2o=";
  };
in
stdenv.mkDerivation {
  pname = "harmony";
  version = "2.6.1";

  buildInputs = [ idris2 ];

  src = ./.;

  buildPhase = ''
    runHook preBuild

    ln -s ${nodeDependencies}/lib/node_modules ./node_modules
    export PATH="${nodeDependencies}/bin:$PATH"
    export IDRIS_ADDS_SRC="${idrisAddsSrc}"

    make build

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp harmony $out/bin/

    runHook postInstall
  '';

}
