# gf test - Goldfish Scheme Test Runner

## Usage

```bash
gf test [PATH|PATTERN]
```

## Examples

```bash
# Run all tests
gf test

# Run tests in a specific directory
gf test tests/liii/string

# Run a specific test file
gf test tests/liii/string-test.scm

# Run tests matching a pattern
gf test string
```

## Description

The `test` command runs all `*-test.scm` files under the `tests/` directory.
You can filter tests by:

- **Directory**: `gf test tests/liii/string` runs all tests in that directory
- **File**: `gf test tests/liii/string-test.scm` runs a specific test file
- **Pattern**: `gf test string` runs tests whose path contains "string"

## Notes

- Test files must end with `-test.scm`
- The command returns exit code 0 if all tests pass, non-zero otherwise
