{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "HROOT-dev";

  buildInputs = [ root ];
  
}
