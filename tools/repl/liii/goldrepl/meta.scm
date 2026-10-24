;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii goldrepl meta)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii list)
    (liii string)
    (liii goldrepl style)
    (liii goldrepl complete)
  ) ;import
  (export *repl-meta-commands* %repl-meta-help %repl-meta-apropos
    %repl-meta-describe %repl-handle-meta
  ) ;export
  (begin

    (define *repl-meta-commands*
      '((",quit" "exit REPL" #t)
        (",q" "exit REPL" #t)
        (",help" "show this help" #t)
        (",?" "show this help" #t)
        (",import" "import Scheme module" #f)
        (",apropos" "search symbols by substring" #f)
        (",a" "search symbols by substring" #f)
        (",describe" "describe symbol" #f)
        (",d" "describe symbol" #f))
    ) ;define

    (define (%repl-meta-help)
      (cons (%repl-styled "bold" "Meta commands:")
        (map
          (lambda (cmd)
            (string-append (%repl-styled "bold" (string-pad-right (car cmd) 16))
              " "
              (cadr cmd)
            ) ;string-append
          ) ;lambda
          *repl-meta-commands*
        ) ;map
      ) ;cons
    ) ;define

    (define (%repl-meta-apropos arg)
      (if (or (not arg) (string-null? arg))
        (list (string-append (%repl-styled "bold" "Usage:") " ,apropos <substring>"))
        (let ((found
                (filter (lambda (entry) (string-contains (car entry) arg)) *repl-symbols*)
              ) ;found
             ) ;
          (if (null? found)
            (list (%repl-styled "dim" (string-append "No symbol matches '" arg "'")))
            (map
              (lambda (entry)
                (string-append (%repl-styled "symbol" (car entry))
                  " "
                  (%repl-styled "dim" "(procedure)")
                  " "
                  (cdr entry)
                ) ;string-append
              ) ;lambda
              found
            ) ;map
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%repl-proc-arity-line val)
      (let* ((arity-info (arity val))
             (min-args (car arity-info))
             (max-args (cdr arity-info))
             (max-str (if (>= max-args 536870912) "any" (number->string max-args)))
            ) ;
        (string-append "  "
          (%repl-gray "Arity:")
          " min "
          (%repl-styled "number" (number->string min-args))
          ", max "
          (%repl-styled "number" max-str)
        ) ;string-append
      ) ;let*
    ) ;define

    (define (%repl-proc-sig-lines val)
      (let ((sig (signature val)))
        (if (and sig (not (null? sig)))
          (list (string-append "  " (%repl-gray "Signature:") " " (object->string sig)))
          '()
        ) ;if
      ) ;let
    ) ;define

    (define (%repl-proc-doc-lines val)
      (let ((doc (catch #t (lambda () (or (documentation val) "")) (lambda (t i) ""))))
        (if (string-null? doc)
          '()
          (list (string-append "  " (%repl-gray (string-append "Doc: " doc))))
        ) ;if
      ) ;let
    ) ;define

    (define (%repl-describe-proc val lines)
      (let ((arity-line (%repl-proc-arity-line val))
            (sig-lines (%repl-proc-sig-lines val))
            (doc-lines (%repl-proc-doc-lines val))
           ) ;
        (append lines (list arity-line) sig-lines doc-lines)
      ) ;let
    ) ;define

    (define (%repl-describe-val val lines)
      (let* ((s (object->string val))
             (preview (if (> (string-length s) 80) (string-append (substring s 0 80) "...") s)
             ) ;preview
            ) ;
        (append lines
          (list (string-append "  " (%repl-gray (string-append "Value: " preview))))
        ) ;append
      ) ;let*
    ) ;define

    (define (%repl-meta-describe arg)
      (cond
       ((or (not arg) (string-null? arg))
        (list (string-append (%repl-styled "bold" "Usage:") " ,describe <symbol>"))
       ) ;
       ((not (defined? (string->symbol arg) (rootlet)))
        (list (%repl-styled "dim" (string-append "Symbol not defined: " arg)))
       ) ;
       (else
         (let* ((sym (string->symbol arg))
                (val (eval sym (rootlet)))
                (type-str (object->string (type-of val)))
                (header (string-append (%repl-styled "bold" arg)
                          " "
                          (%repl-styled "dim" (string-append "(" type-str ")"))
                        ) ;string-append
                ) ;header
                (lines (list header))
               ) ;
           (if (procedure? val)
             (%repl-describe-proc val lines)
             (%repl-describe-val val lines)
           ) ;if
         ) ;let*
       ) ;else
      ) ;cond
    ) ;define

    (define (%repl-dispatch-meta name arg)
      (cond ((member name '(",quit" ",q")) (values #t '()))
            ((member name '(",help" ",?")) (values #f (%repl-meta-help)))
            ((string=? name ",import")
             (if (or (not arg) (string-null? arg))
               (values #f
                 (list (string-append (%repl-styled "error" "Usage:") " ,import <module>"))
               ) ;values
               (values 'import (list (string-append "(import " arg ")")))
             ) ;if
            ) ;
            ((member name '(",apropos" ",a")) (values #f (%repl-meta-apropos arg)))
            (else (values #f (%repl-meta-describe arg)))
      ) ;cond
    ) ;define

    (define (%repl-meta-match? input name exact?)
      (if exact?
        (string=? input name)
        (and (string-starts? input name) (> (string-length input) (string-length name)))
      ) ;if
    ) ;define

    (define (%repl-handle-meta input)
      (let ((cmd
              (find (lambda (c) (%repl-meta-match? input (car c) (caddr c)))
                *repl-meta-commands*
              ) ;find
            ) ;cmd
           ) ;
        (if cmd
          (let* ((name (car cmd))
                 (exact? (caddr cmd))
                 (arg
                   (if exact? #f (string-trim-both (substring input (string-length name))))
                 ) ;arg
                ) ;
            (%repl-dispatch-meta name arg)
          ) ;let*
          (values #f
            (list (%repl-styled "error" (string-append "Unknown meta command: " input)))
          ) ;values
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
