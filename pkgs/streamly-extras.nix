{  mkDerivation, fetchgit, stdenv
 , base, containers, stm, streamly
}:
mkDerivation {
  pname = "streamly-extras";
  version = "0.0.1";
  src = fetchgit {
    url = "https://github.com/juspay/streamly-extras";
    sha256 = "0ib0qvnkpzdzalxig4f9bnx5mkd4sri9sdrm403h6rxj18kkfr8k";
    rev = "faea671b14560726ff780136828a57b3be8cdc47";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base containers stm streamly ];
  homepage = "https://github.com/juspay/streamly-extras";
  description = "To provide extra utility functions on top of Streamly";
  license = stdenv.lib.licenses.bsd3;
  isLibrary = true;
}
