# gf test - Goldfish Scheme Test Runner

## Usage

```bash
gf test [options] [PATH|PATTERN]
```

## Examples

```bash
# Run the full suite
gf test --all

# Run tests in a directory (auto-detects tests/ in path)
gf test tools/doc/tests/
gf test tools/doc/tests/liii/golddoc/

# Run tests in project tests directory
gf test tests/liii/string/

# Run a specific test file
gf test string-test.scm
gf test tests/liii/json-test.scm

# Run tests matching a pattern
gf test string

# Run only test files changed since a git revision
gf test --changed-since=HEAD
gf test --changed-since=main
```

## Description

With a path or pattern, `test` runs matching `*-test.scm` files. With no
arguments, it runs tests changed since `main` on non-main branches; use
`--all` for the full suite.

### Auto-Detection of tests/ Directory

If the path contains `/tests/`, the command will automatically switch to the parent
directory before running tests:

- `gf test tools/doc/tests/` - Changes to `tools/doc/` and runs tests from `tests/`
- `gf test tools/doc/tests/liii/golddoc/` - Changes to `tools/doc/` and runs tests from `tests/liii/golddoc/`

This is equivalent to:
```bash
cd tools/doc && gf test tests/
```

### Filter Options

You can filter tests by:

- **Directory**: `gf test tests/liii/string/` runs all tests in that directory
- **File**: `gf test string-test.scm` runs a specific test file
- **Pattern**: `gf test string` runs tests whose path contains "string"
- **Changed files**: `gf test --changed-since=HEAD` runs changed `*-test.scm` files

`--changed-since` uses `git diff --name-only` and does not include changed dependents.

## Notes

- Test files must end with `-test.scm`
- The command returns exit code 0 if all tests pass, non-zero otherwise
- Set `GOLDFISH_TEST_TIMEOUT=300` to bound each isolated file to 300 seconds
  (including process startup). GNU `timeout` is required. Persistent worker
  chunks receive the file limit multiplied by their size; incomplete chunks
  fall back to individually bounded runs. A timeout remains a failed test,
  never a skip. With this option, serial runs use subprocesses rather than
  unbounded native forks. Unset preserves the existing behavior.

## Native runtime regressions

Native bootstrap and tiny-reader regressions use the native entry point:

```bash
nix develop -c sh tools/test-native.sh
```

The same suite is also available as an explicit xmake target:

```bash
nix develop -c xmake build native-test
```

The regular Scheme suite runs through the native `gf test` command.
