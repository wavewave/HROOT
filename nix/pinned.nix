{ pkgs }:

{
  fficxxSrc =
    pkgs.fetchgit {
      url = "git://github.com/wavewave/fficxx.git";
      rev = "488b3700bdde741dd856ade63439fee4062ea9b3";
      sha256 = "0m7j06s5vkxdzsk0llspdppmj8vrs80fi84lwj0szj37gdnmq233";
    };
}
