# gf source - Goldfish Scheme Source Viewer

## Usage

```bash
gf source ORG/LIB
```

## Examples

```bash
# Display source code of a library
gf source liii/path

# Display source code with specific function
gf source liii/string
```

## Description

The `source` command prints the exact source code of a library from the current
`*load-path*`. It reads the real library file, not test files or generated docs.

## Notes

- The library must be in the current `*load-path*`
- The source is displayed as-is from the .scm file
- This is useful for inspecting implementation details
