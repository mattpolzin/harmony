{
  buildIdris',
}:
let
  pkg = buildIdris' {
    ipkgName = "harmony-tests";
    src = builtins.path {
      path = ./.;
      name = "harmony-tests-src";
    };

    postInstall = ''
      wrapProgram $out/bin/harmony-test \
        --prefix IDRIS2_PACKAGE_PATH : ${pkg.IDRIS2_PACKAGE_PATH}
    '';

    meta.mainProgram = "harmony-test";
  };
in
pkg
