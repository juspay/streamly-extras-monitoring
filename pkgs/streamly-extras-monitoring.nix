{   mkDerivation, stdenv
  , base, basic-prelude, exceptions, monad-log, mtl, prometheus-client, streamly
  , streamly-extras, text, wai, wai-middleware-prometheus, warp
}:
mkDerivation {
  pname = "streamly-extras-monitoring";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  # TODO: make executable
  # isExecutable = true;
  libraryHaskellDepends = [
    base basic-prelude exceptions monad-log mtl prometheus-client streamly streamly-extras
    text wai wai-middleware-prometheus warp
  ];
  homepage = "https://github.com/juspay/streamly-extras-monitoring";
  description = "Helper functions for logging and metrics of streams.";
  license = stdenv.lib.licenses.bsd3;
}
