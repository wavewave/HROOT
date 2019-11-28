{ pkgs }:

{
  fficxxSrc =
    pkgs.fetchgit {
      url = "git://github.com/wavewave/fficxx.git";
      rev = "418571b3812b630438dd426384012ec6645ad7cc";
      sha256 = "1zy3idlh4a5cax9amj7pgbj9bd9bs51ikdjc7z4ym5wg5yg3mpn0";
    };
}