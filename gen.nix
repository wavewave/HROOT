{ stdenv }:

hself:

let

  hsenv = hself.ghcWithPackages (p: [ p.HROOT-generate ]);

in stdenv.mkDerivation {
  name = "HROOT-src";
  buildInputs = [ hsenv ];
  src = ./.;
  buildPhase = ''
    HROOT-generate
  '';
  installPhase = ''
    mkdir -p $out
    cp -a HROOT HROOT-core HROOT-graf HROOT-hist HROOT-io HROOT-math HROOT-net HROOT-tree HROOT-RooFit HROOT-RooFit-RooStats $out
  '';
}
