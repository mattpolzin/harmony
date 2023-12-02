{ stdenv }:
stdenv.mkDerivation {
  pname = "harmony";
  version = "2.6.1";

  src = ./.;
}
