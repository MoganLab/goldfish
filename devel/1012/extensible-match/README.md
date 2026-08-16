# Extensible Match

This Scheme library provides a powerful generalized pattern matching construct which can be extended with new types of patterns using Scheme’s macro system. It is the sample implementation for [SRFI 262](https://srfi.schemers.org/srfi-262/srfi-262.html), which see for documentation.

## Installation and use

This library is currently written for R6RS Scheme implementations only.

The `(extensible-match)` library provides the interface described by the SRFI. The `(srfi :262 match)` library is an alias for this library. In future, `(extensible-match)` may be extended to include features which are not in the SRFI, while `(srfi :262 match)` will remain stable once the SRFI is finalized.

This library will eventually be packaged and made available in [Akku](https://akkuscm.org/), but isn’t yet.

### Chez Scheme

Install Aaron Hsu’s [`chez-srfi`](https://github.com/arcfide/chez-srfi) and place this repository’s contents within your Chez Scheme library path.

Note that Chez Scheme currently (version 10.3.0) has a bug in the use of identifier properties when imported from a library with qualified imports. See [Chez issue 929](https://github.com/cisco/ChezScheme/issues/929) and [patch 987.](https://github.com/cisco/ChezScheme/pull/987) In order to work around it, you may need to exclude duplicate imports even if they don’t formally conflict at the binding level. You can also import `(extensible-match)` with a prefix.

### Guile

*Note:* The implementation of pattern syntax for Guile currently has a bug in an edge case. If you import the same binding multiple times under different names, and then apply pattern syntax to one of those names, the pattern syntax will be attached to all of them. I think it is possible to fix this, but I need to think harder about it. Hopefully, some future version of Guile will make the implementation-specific definition and lookup module obsolete anyway.

#### Directly from this repository

Guile must be started with `--r6rs`. If you only want to use the library, you will also need the modules from `./impl-lib/guile/srfi`.

If you want to run the test suite, Guile must be started with `--r7rs --r6rs` (*not* `--r6rs --r7rs`), and you will also need the modules from `./impl-lib/guile/chibi`.

#### Through Guix

Thanks to Zhu Zihao (@readevalprintloop on Codeberg), a version of extensible-match is [now packaged in Guix under the name `guile-extensible-match`.](https://packages.guix.gnu.org/packages/guile-extensible-match/) This version does not need the `--r6rs` nor `--r7rs` flags.

Zhu is responsible for the maintenance of the Guix package itself. If you are using his version and find a bug, please try and reproduce it in the version from this repository first before reporting it here; if you can’t reproduce it here, report it to Guix, not to this repository.

### Other implementations of Scheme

No support yet :-( Pester the maintainer of your favourite R6RS implementation to add support for [SRFI 213](https://srfi.schemers.org/srfi-213/srfi-213.html).

Support for R7RS implementations which use `syntax-case` (and which support identifier properties) will also come some day.

## Future plans

At around 5,500 source lines of code, of which 1,700 are tests, this is one of the larger SRFI sample implementations. The ratio of test to implementation lines of code is likely to increase in the immediate future for two reasons: first, I would like to refactor away the repetitive code for record-type definitions, equivalence predicates, and hash functions, which will hopefully reduce the number of implementation lines of code; second, I would like to add more tests, including a property-based testing suite, plus tests for some internal procedures which are currently only indirectly tested.

The SRFI is not yet finalized, and changes based on developments during the draft period are possible.

Otherwise, see the issue tracker for plans.

## Contributing

Bug reports and feature requests are welcome, but patches are accepted by invitation only. That means if you want to submit a patch, please talk to me first about what you intend to change before you start hacking.

## Miscellaneous information

This library is placed under the CC0 1.0 Universal legal code. This does not generally apply to files in the `impl-lib` directory, which are by other authors.
