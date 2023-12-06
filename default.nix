{ stdenv, lib, callPackage, fetchFromGitHub, idris2, makeWrapper, nodejs }:
let 
  nodeDependencies = (callPackage ./node2nix.nix { inherit nodejs; }).nodeDependencies;
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
  version = "2.6.2";

  nativeBuildInputs = [ idris2 makeWrapper ];
  buildInputs = [ nodejs ];

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

    wrapProgram $out/bin/harmony \
      --prefix PATH : ${lib.makeBinPath [ nodeDependencies ]} \
      --prefix NODE_PATH : ${nodeDependencies}/lib/node_modules

    runHook postInstall
  '';

}
