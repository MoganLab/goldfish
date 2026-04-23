Unknown command: tests/liii/string-cursor/reverse-list-

Goldfish Scheme 17.11.55 by LiiiLabs

Commands:
  help               Display this help message
  version            Display version
  eval CODE          Evaluate Scheme code
                     Example: gf eval '(+ 1 2)'
                     Prefer single quotes so double quotes inside Scheme strings usually do not need escaping
  load FILE          Load Scheme code from FILE, then enter REPL
  fix [options] PATH Format PATH (PATH can be a .scm file or directory)
                     Options:
                       --dry-run  Print formatted result to stdout
  source ORG/LIB     Print the exact source of ORG/LIB from current *load-path*
                     Reads the real library file, not tests/ or generated docs
                     Example: gf source liii/path
  doc ORG/LIB        Show the library overview for ORG/LIB from tests/
                     Usually reads tests/ORG/LIB-test.scm
                     Example: gf doc liii/path
  doc ORG/LIB FUNC   Show the function doc/test file for FUNC under a specific library
                     Best when you already know the library, or the name is ambiguous
                     Example: gf doc liii/path "path-read-text"
                     Quote FUNC for names like "bag-delete!", "path?", "alist->fxmapping", or "bag<=?"
                     This preserves symbols such as ! ? > < and keeps FUNC as one shell argument
  doc FUNC           Search visible libraries for exported FUNC, then show its doc/test file
                     If multiple libraries export it, candidates are listed
                     Example: gf doc "string-split"
                     Quote FUNC for names like "bag-delete!", "path?", "alist->fxmapping", or "bag<=?"
                     This keeps shell-sensitive symbols intact and makes it clear FUNC is one argument
  doc --build-json   Rebuild tests/function-library-index.json for global gf doc FUNC lookup
                     Needed by function-name search and fuzzy suggestions
                     Run this after changing exports, or before packaging
  test [PATTERN]     Run tests (all *-test.scm files under tests/)
                     PATTERN can be:
                       (none)          Run all tests
                       FILE.scm        Run specific test file
                       DIR/            Run tests in directory
                       name-test.scm   Match by file name
                       substring       Match by path substring
  run TARGET         Run main function from TARGET
                     TARGET can be:
                       FILE.scm       Load file and run main
                       x/y/z.scm      Load file and run main
                       module.name    Import (module name) and run main
  FILE               Load and evaluate Scheme code from FILE

Options:
  --mode, -m MODE    Set mode: default, liii, sicp, r7rs, s7
  -I DIR             Prepend DIR to library search path
  -A DIR             Append DIR to library search path

If no command is specified, help is displayed by default.
