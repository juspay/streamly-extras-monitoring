# Revision history for streamly-extras
#### Follows Semver versioning (semver.org)
Version Format: Major.Minor.Patch-prerelease

## 0.0.1 -- 2019-10-29
* First version. Released on an unsuspecting world.

## 0.1.0 -- 2019-10-31
### Backward compatible API Changes
* Adds `doAt` function
* In `streamlyInfoLogger`, metric update for the gauge sets the gauge to rate/sec rather than rate/samplingTimeInterval
### Non-API Changes
* Updates nix files
* Updates ghc options

## 1.0.0 -- 2019-11-04
### Breaking API Changes
* Takes a `maybeOp` with each metric and applies that to the `n` before using it

## 1.0.1 -- 2019-11-04
### Backward compatible API Changes
* Fixes logging inside `streamlyInfoLogger`

## 2.0.0 -- 2019-11-07
### Breaking API Changes
* Adds `vector` and `label` support - Major design change

## 2.0.1 -- 2019-11-08
### Non-API Changes
* Updates `streamly` and `streamly-extras` versions

## 2.1.0 -- 2019-11-12
### Non-breaking API Changes
* Adds `subGauge` and `decGauge` functions
### Non-API Changes
* Updates `streamly` version
* Bugfix: Logs once instead of twice - Logs only for gauges, instead of both counters and gauges
