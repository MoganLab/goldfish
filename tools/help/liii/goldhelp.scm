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

(define-library (liii goldhelp)
  (import (scheme base)
    (scheme file)
    (scheme write)
    (scheme process-context)
    (liii sort)
    (liii string)
    (liii argparse)
    (liii path)
    (liii os)
    (liii error)
    (liii sys)
    (liii list)
    (liii project)
  ) ;import
  (export main load-gfproject get-tool-description display-help)
  (begin

    (define (load-gfproject)
      (gfproject-tools)
    ) ;define

    (define (get-tool-description tools tool-name lang)
      (let* ((tool (assq (string->symbol tool-name) tools))
             (desc-alist (and tool (assq 'description (cdr tool))))
             (desc (and desc-alist (cdr desc-alist))))
        (if (not desc) ""
          (let ((lang-desc (assq (string->symbol lang) desc))
                (en-desc (assq 'en_US desc)))
            (cond [lang-desc (let ((v (cdr lang-desc))) (if (string? v) v ""))]
                  [en-desc (let ((v (cdr en-desc))) (if (string? v) v ""))]
                  [else ""])))))

    (define (display-version)
      (display "Goldfish Scheme ")
      (display (version))
      (display " by LiiiLabs")
      (newline)
    ) ;define

    (define (display-command-line cmd desc . extra-lines)
      (display "  ")
      (display cmd)
      (let ((pad (- 19 (+ (string-length "  ") (string-length cmd)))))
        (if (> pad 0)
          (display (make-string pad #\space))
          (begin
            (newline)
            (display (make-string 19 #\space))
          ) ;begin
        ) ;if
      ) ;let
      (display desc)
      (newline)
      (for-each (lambda (line) (display (make-string 19 #\space)) (display line) (newline))
        extra-lines
      ) ;for-each
    ) ;define

    (define (display-dynamic-commands tools)
      (let ((tool-names (list-sort string<? (map (lambda (kv) (symbol->string (car kv))) tools))))
        (for-each (lambda (tool-name)
                    (let ((desc (get-tool-description tools tool-name "en_US")))
                      (display-command-line tool-name desc)
                    ) ;let
                  ) ;lambda
          tool-names
        ) ;for-each
      ) ;let
    ) ;define

    (define (display-help)
      (let* ((tools (load-gfproject)))
        (display-version)
        (newline)
        (display "Commands:")
        (newline)
        (let ((help-desc (get-tool-description tools "help" "en_US")))
          (display-command-line "help" help-desc)
        ) ;let
        (let ((other-tools (filter (lambda (kv) (not (eq? (car kv) 'help))) tools)))
          (when (not (null? other-tools))
            (display-dynamic-commands other-tools)))
        (display-command-line "FILE" "Load and evaluate Scheme code from FILE")
        (newline)
        (display "Options:")
        (newline)
        (display-command-line "--mode, -m MODE"
          "Set mode: default, liii, sicp, r7rs, s7"
        ) ;display-command-line
        (display-command-line "-I DIR" "Prepend DIR to library search path")
        (display-command-line "-A DIR" "Append DIR to library search path")
        (display-command-line "-e CODE" "Alias for eval CODE")
        (newline)
        (display "Type 'gf help <command>' for more information on a specific command.")
        (newline)
      ) ;let*
    ) ;define

    (define (find-tool-readme tool-name)
      (let ((cwd (getcwd)))
        (if cwd
          (let ((readme-path (path->string (path-join (path cwd) (path "tools") (path tool-name) (path "README.md"))))) ;
            (if (file-exists? readme-path) readme-path #f)
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (display-tool-help tool-name)
      (let* ((tools (load-gfproject))
             (tool (assq (string->symbol tool-name) tools)))
        (if (not tool)
          (begin
            (display "Unknown command: ")
            (display tool-name)
            (newline)
          ) ;begin
          (let ((readme-path (find-tool-readme tool-name)))
            (if readme-path
              (begin
                (display (path-read-text readme-path))
                (newline)
              ) ;begin
              (let ((cmd (string-append "gf " tool-name " --help")))
                (os-call cmd)
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (make-help-arg-parser)
      (make-argument-parser '((command . "help")
                              (skip-value-options "-m" "--mode" "-I" "-A")
                              (skip-prefix-options "-m=" "--mode=")
                              (unknown-options . positional))
      ) ;make-argument-parser
    ) ;define

    (define (main)
      (let ((parser (make-help-arg-parser)))
        (parser :parse-argv (command-line))
        (let ((positionals (parser :positionals)))
          (if (null? positionals) (display-help) (display-tool-help (car positionals)))
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
