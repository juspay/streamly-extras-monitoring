# Revision history for streamly-extras
#### Follows Semver versioning (semver.org)
Version Format: Major.Minor.Patch-prerelease

## 0.0.1 -- 2019-10-29
* First version. Released on an unsuspecting world.

## 0.1.0 -- 2019-10-31
### API changes
* Adds `doAt` function
* In `streamlyInfoLogger`, metric update for the gauge sets the gauge to rate/sec rather than rate/samplingTimeInterval
### Other changes
* Updates nix files
* Updates ghc options

## 1.0.0 -- 2019-11-04
* Takes a `maybeOp` with each metric and applies that to the `n` before using it

## 1.0.1 -- 2019-11-04
* Fixes logging inside `streamlyInfoLogger`

## 2.0.0 -- 2019-11-07
* Adds `vector` and `label` support - Major design change

## 2.0.1 -- 2019-11-08
* Updates `streamly` and `streamly-extras` versions
