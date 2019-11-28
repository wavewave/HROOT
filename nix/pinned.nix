{ pkgs }:

{
  fficxxSrc =
    pkgs.fetchgit {
      url = "git://github.com/wavewave/fficxx.git";
      rev = "a7935d39b7c42fd25332523c653a44061747db97";
      sha256 = "1k9kvff1vrdzmk0mv8bf1cnzgynn3kxkii5mc57hq3d3s0g3xc3x";
    };
}
