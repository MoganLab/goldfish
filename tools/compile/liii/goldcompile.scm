;;
;; gf compile -- compile scheme sources into the goldfish cache (guild compile)
;;
;; For each FILE, compile its whole transitive library closure into the
;; ccache without executing the file's own top-level forms.  A user (or an
;; installer script) runs this once after installing a program; every later
;; run then hits the cache and pays only interpreter startup.  Mirror of
;; Guile's `guild compile`, whose default output is the compile cache.
;;
;; The cache root is the usual ~/.cache/goldfish/ccache unless
;; GOLDFISH_CACHE_DIR points elsewhere (read-only prebuilt caches pair it
;; with GOLDFISH_CACHE_READONLY).
;;
;; The warming itself runs in a fresh `gf` subprocess (the tool library
;; cannot reach the expander's module-define! API): the subprocess imports
;; (goldfish), whose program surface exposes warm-file! / compile-file-cached.

(define-library (liii goldcompile)
  (import (scheme base)
    (liii argparse)
    (liii sys)
    (liii os)
  ) ;import
  (export main)
  (begin

    (define (compile-one file)
      (let* ((tag (number->string (getpid)))
             (script (string-append (os-temp-dir) "/gf-compile-" tag ".scm"))
             (cmd (string-append (executable) " -m liii '" script "'")))
        (call-with-output-file script
          (lambda (p)
            (display "(import (goldfish))\n" p)
            (display "(warm-file! " p)
            (write file p)
            (display ")\n" p)
            (display "(catch #t\n" p)
            (display "  (lambda () (compile-file-cached " p)
            (write file p)
            (display "))\n" p)
            (display "  (lambda (tag . info) #f))\n" p)))
        (display "compile: ") (display file) (newline)
        (let ((out (os-call cmd)))
          (when (and (number? out) (not (= out 0)))
            (display (string-append "gf compile: subprocess failed ("
                                    (number->string out) ")\n")
                     (current-error-port))))
        (os-call (string-append "rm -f '" script "'"))))

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
