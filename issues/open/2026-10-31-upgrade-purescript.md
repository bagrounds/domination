# Upgrade to latest PureScript version

**Source:** Technical debt

**Status:** Open

## Description

The project is currently on PureScript 0.14.1 (package set from May 6, 2021). The latest PureScript version is 0.15.x, which includes significant improvements but also breaking changes.

## Current State

- PureScript compiler: `^0.14.1`
- Spago: `^0.15.3`
- Package set: `psc-0.14.1-20210506`

## Migration Plan

### Phase 1: Assess Dependencies
- `halogen` - likely has 0.15.x compatible version
- `argonaut-generic` - likely has 0.15.x compatible version
- `arraybuffer-class` - custom fork (`bagrounds/purescript-arraybuffer-class`), will need updating
- `webaudio` (`adkelley/purescript-webaudio v0.2.1`) - may be unmaintained, may need our own fork
- `arraybuffer` (`jacereda/purescript-arraybuffer v10.0.2`) - check for 0.15.x compatibility
- `float32` (`athanclark/purescript-float32 v0.2.0`) - check for 0.15.x compatibility
- `uint` (`zaquest/purescript-uint v5.1.4`) - check for 0.15.x compatibility

### Phase 2: Breaking Changes in 0.15.x
- `Prim.Row` changes
- `RowList` moved to `Prim.RowList`
- `Polykinds` support changes
- `ES modules` output (instead of CommonJS)
- Various standard library changes

### Phase 3: Update Build Tools
- Update spago (possibly to spago-next/spago 0.20+)
- Update parcel if needed for ES module output
- Update build scripts

### Phase 4: Rewrite Unmaintained Dependencies
Dependencies that may need our own implementation:
- `webaudio` - Write minimal Web Audio API FFI bindings
- `arraybuffer-class` - Already a custom fork, update for 0.15.x
- `float32` - Simple module, easy to rewrite if needed

### Phase 5: Test and Deploy
- Run full test suite
- Test in browser
- Deploy to staging
- Verify peer-to-peer functionality still works
