# Revision history for transformers-eff

## 0.2.1.0  -- 2018-03-21

* Upper bounds have been removed. This is due to lack of maintainer time.

## 0.2.0.0  -- 2017-02-07

### Breaking Changes

* The type of `gets` was changed to actually be the `gets` operation.
* `Control.Effect.Nondeterminism` now uses `list-transformer` instead of lists.
* `Control.Effect.Nondeterminism.choose` now chooses from any `Foldable` 
  structure.

### Non-breaking Changes

* A `MonadIO` instance was added to `Eff`.
* New function: `Control.Effect.Nondeterminism.runNondeterminismM`.

### Other changes

* The upper bound of `base` was increased.

## 0.1.0.0  -- 2017-02-02

* First version. Released on an unsuspecting world. Mwahaha.
