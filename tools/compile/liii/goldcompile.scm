;;
;; gf compile -- compile scheme sources into the goldfish cache (guild compile)
;;
;; For each FILE, compile its transitive library closure into the ccache
;; without executing the file's own top-level forms.  A user (or an
;; installer script) runs this once after installing a program; every later
;; run then hits the cache and pays only interpreter startup.
;;
;; The cache root is the usual ~/.cache/goldfish/ccache unless
;; GOLDFISH_CACHE_DIR points elsewhere (read-only prebuilt caches pair it
;; with GOLDFISH_CACHE_READONLY).
;;
;; warm-file! lives in the base module; it is also mirrored into the host
;; rootlet (module.scm) so this tool library can call it bare -- importing
;; (goldfish) would bind the name to a module toplevel that a tool library
;; cannot resolve.

(define-library (liii goldcompile)
  (import (scheme base)
    (liii argparse)
    (liii sys)
  ) ;import
  (export main)
  (begin

    (define (compile-one file)
      (display "compile: ") (display file) (newline)
      (catch #t
        (lambda ()
          (let ((warmed (warm-file! file)))
            (display "  libraries: ")
            (display (length warmed))
            (newline)))
        (lambda (tag . info)
          (display "gf compile: failed on " (current-error-port))
          (display file (current-error-port))
          (newline (current-error-port))
          (exit -1))))

    (define (main)
      "Main entry point for the compile command"
      (let ((parser (make-argument-parser '((command . "compile")
                                            (unknown-options . positional)))
            ) ;parser
           ) ;
        (parser :parse-argv (argv))
        (let ((files (parser :positionals)))
          (if (null? files)
            (begin
              (display "gf compile: no input files\n" (current-error-port))
              (exit -1))
            (for-each compile-one files))
        ) ;let-files
      ) ;let-parser
    ) ;define

  ) ;begin
) ;define-library
