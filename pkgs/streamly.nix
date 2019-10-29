{ mkDerivation, atomic-primops, base, containers, deepseq
, exceptions, fetchgit, gauge, ghc-prim, heaps, hspec
, lockfree-queue, monad-control, mtl, network, QuickCheck, random
, stdenv, transformers, transformers-base, typed-process
}:
mkDerivation {
  pname = "streamly";
  version = "0.6.1";
  src = fetchgit {
    url = "https://github.com/composewell/streamly.git";
    sha256 = "0qrsq2bnwchs8zjl4icazwhp76nlgfq8wbag2ip221y6h30vmrxi";
    rev = "8a4d965a93b375cb62afb66b74580c751f2ce3ff";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    atomic-primops base containers deepseq exceptions ghc-prim heaps
    lockfree-queue monad-control mtl network transformers
    transformers-base
  ];
  testHaskellDepends = [
    base containers exceptions hspec mtl QuickCheck random transformers
  ];
  benchmarkHaskellDepends = [
    base deepseq gauge random typed-process
  ];
  homepage = "https://github.com/composewell/streamly";
  description = "Beautiful Streaming, Concurrent and Reactive Composition";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
  doHaddock = true;
}
