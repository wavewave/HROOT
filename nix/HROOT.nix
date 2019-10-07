{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  config = import ./config.nix { inherit fetchgit; };
  config2 = import ./config2.nix { inherit root; };  
  
  HROOTconf = writeText "HROOT.conf"
    ''
      HROOT-core { 
        scriptbase="/home/wavewave/repo/src/HROOT-generate"
        workingdir="/home/wavewave/repo/workspace/HROOT/working/core"
        installbase="/home/wavewave/repo/workspace/HROOT/HROOT-core"
      }
      HROOT-hist { 
        scriptbase="/home/wavewave/repo/src/HROOT-generate"
        workingdir="/home/wavewave/repo/workspace/HROOT/working/hist"
        installbase="/home/wavewave/repo/workspace/HROOT/HROOT-hist"
      }
      HROOT-tree { 
        scriptbase="/home/wavewave/repo/src/HROOT-generate"
        workingdir="/home/wavewave/repo/workspace/HROOT/working/tree"
        installbase="/home/wavewave/repo/workspace/HROOT/HROOT-tree"
      }
      HROOT-graf { 
        scriptbase="/home/wavewave/repo/src/HROOT-generate"
        workingdir="/home/wavewave/repo/workspace/HROOT/working/graf"
        installbase="/home/wavewave/repo/workspace/HROOT/HROOT-graf"
      }
      HROOT-math { 
        scriptbase="/home/wavewave/repo/src/HROOT-generate"
        workingdir="/home/wavewave/repo/workspace/HROOT/working/math"
        installbase="/home/wavewave/repo/workspace/HROOT/HROOT-math"
      }
      HROOT-io { 
        scriptbase="/home/wavewave/repo/src/HROOT-generate"
        workingdir="/home/wavewave/repo/workspace/HROOT/working/io"
        installbase="/home/wavewave/repo/workspace/HROOT/HROOT-io"
      }
      HROOT-RooFit { 
        scriptbase="/home/wavewave/repo/src/HROOT-generate"
        workingdir="/home/wavewave/repo/workspace/HROOT/working/RooFit"
        installbase="/home/wavewave/repo/workspace/HROOT/HROOT-RooFit"
      }
      HROOT-RooFit-RooStats { 
        scriptbase="/home/wavewave/repo/src/HROOT-generate"
        workingdir="/home/wavewave/repo/workspace/HROOT/working/RooStats"
        installbase="/home/wavewave/repo/workspace/HROOT/HROOT-RooStats"
      }
      HROOT { 
        scriptbase="/home/wavewave/repo/src/HROOT-generate"
        workingdir="/home/wavewave/repo/workspace/HROOT/working/HROOT"
        installbase="/home/wavewave/repo/workspace/HROOT/HROOT"
      }
    '';

  newHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: config self super // config2 self super;
  }; 

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            HROOT fficxx-runtime
          ]);
in

stdenv.mkDerivation {
  name = "HROOT-env";
  buildInputs = [ hsenv root ];
  shellHook = ''
    PS1="\n\[\033[0;35m\][\u@\h.HROOT:\w]\$\[\033[0m\] "
  '';  
}
