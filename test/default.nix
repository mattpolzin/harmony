{
  buildIdris',
  harmony-lib
}:
let
pkg = buildIdris' {
  ipkgName = "harmony-tests";
  src = builtins.path {
    path = ./.;
    name = "harmony-tests-src";
  };

  extraIdrisLibraries = [ harmony-lib ];

  postInstall = ''
    wrapProgram $out/bin/harmony-test \
      --prefix IDRIS2_PACKAGE_PATH : ${pkg.IDRIS2_PACKAGE_PATH}
  '';

  meta.mainProgram = "harmony-test";
};
in pkg
