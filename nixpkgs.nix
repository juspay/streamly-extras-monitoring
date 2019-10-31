let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/e8a36cdf57193e56514aa6eeff936159372f0ace.tar.gz";
    sha256 = "1jxdqphacpzkvwpkw67w1222jnmyplzall4n9sdwznyipxz6bqsv";
  };
in nixpkgs
