# gf test - Goldfish Scheme Test Runner

## Usage

```bash
gf test [TARGET] [PATH|PATTERN]
```

## Examples

```bash
# Run all tests
gf test

# Run tests in a target directory (requires TARGET/tests)
gf test tools/doc

# Run tests in a specific directory
gf test tests/liii/string

# Run a specific test file
gf test string-test.scm

# Run tests matching a pattern
gf test string
```

## Description

The `test` command runs all `*-test.scm` files under the `tests/` directory.

### Target Support

If the first argument is a directory containing a `tests` subdirectory, the command
will switch to that directory before running tests:

- `gf test tools/doc` - Changes to `tools/doc` and runs tests from `tools/doc/tests/`

You can combine target with filters:
- `gf test tools/doc string` - Run tests in `tools/doc/tests/` matching "string"

### Filter Options

You can filter tests by:

- **Directory**: `gf test tests/liii/string` runs all tests in that directory
- **File**: `gf test string-test.scm` runs a specific test file
- **Pattern**: `gf test string` runs tests whose path contains "string"

## Notes

- Test files must end with `-test.scm`
- The command returns exit code 0 if all tests pass, non-zero otherwise
