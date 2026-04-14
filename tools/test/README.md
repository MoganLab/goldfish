# gf test - Goldfish Scheme Test Runner

## Usage

```bash
gf test [PATH|PATTERN]
```

## Examples

```bash
# Run all tests
gf test

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
```

## Description

The `test` command runs all `*-test.scm` files under the `tests/` directory.

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

## Notes

- Test files must end with `-test.scm`
- The command returns exit code 0 if all tests pass, non-zero otherwise
