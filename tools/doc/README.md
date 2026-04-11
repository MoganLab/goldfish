# gf doc - Goldfish Scheme Documentation Tool

## Usage

```bash
gf doc ORG/LIB
gf doc ORG/LIB "FUNC"
gf doc "FUNC"
gf doc --build-json
```

## Examples

```bash
# Display library documentation
gf doc liii/path

# Display function documentation within a library
gf doc liii/path "path-read-text"

# Search for a function across all libraries
gf doc "string-split"

# Fuzzy match - suggests similar function names
gf doc "string-spli"

# Rebuild function index (run after test file changes)
gf doc --build-json
```

## Description

The `doc` command browses Goldfish Scheme library documentation by reading
test files from the `tests/` directory. It provides:

- Library overview documentation
- Function-specific documentation with examples
- Fuzzy search for function names
- Global function name indexing

## Function Name Mapping Rules

Special characters in function names are mapped to file names:

| Character | Mapping    | Example                          |
|-----------|------------|----------------------------------|
| +         | plus       | + -> plus                        |
| -         | minus      | - -> minus                       |
| *         | star       | * -> star                        |
| /         | slash      | / -> slash                       |
| =         | eq         | = -> eq                          |
| <         | lt         | < -> lt                          |
| <=        | le         | <= -> le                         |
| >         | gt         | > -> gt                          |
| >=        | ge         | >= -> ge                         |
| ?         | -p         | path? -> path-p                  |
| !         | -bang      | reverse! -> reverse-bang         |
| ->        | -to-       | list->vector -> list-to-vector   |
| /         | -slash-    | path/join -> path-slash-join     |
| *         | -star      | char* -> char-star               |
| =         | -eq        | char= -> char-eq                 |
| <         | -lt        | char< -> char-lt                 |
