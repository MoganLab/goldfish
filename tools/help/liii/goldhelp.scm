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
    (liii njson)
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
      (string->njson (gfproject-load-config))
    ) ;define

    (define (get-tool-description tools tool-name lang)
      "Get description for a tool in specified language"
      (let* ((tool (catch #t (lambda () (njson-ref tools (string-append tool-name ""))) (lambda args 'null)))
             (desc (if (or (eq? tool 'null) (and (njson? tool) (njson-null? tool))) 'null (catch #t (lambda () (njson-ref tool (string-append "description" ""))) (lambda args 'null))))
            ) ;
        (if (or (eq? desc 'null) (and (njson? desc) (njson-null? desc)))
          ""
          (let ((lang-desc (catch #t (lambda () (njson-ref desc (string-append lang ""))) (lambda args 'null))))
            (if (or (eq? lang-desc 'null) (and (njson? lang-desc) (njson-null? lang-desc)))
              (let ((en-desc (catch #t (lambda () (njson-ref desc (string-append "en_US" ""))) (lambda args 'null))))
                (if (or (eq? en-desc 'null) (and (njson? en-desc) (njson-null? en-desc))) "" (if (string? en-desc) en-desc ""))
              ) ;let
              (if (string? lang-desc) lang-desc "")
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (njson-empty? x)
      "Check if njson-ref returned empty result (null)"
      (or (eq? x 'null) (and (njson? x) (njson-null? x)) (and (string? x) (string-null? x)))
    ) ;define

    (define (has-tool-implementation? tools tool-name)
      "Check if a tool has Scheme implementation (has organization and module)"
      (let ((tool (catch #t (lambda () (njson-ref tools (string-append tool-name ""))) (lambda args 'null))))
        (if (or (eq? tool 'null) (and (njson? tool) (njson-null? tool)))
          #f
          (let ((org (catch #t (lambda () (njson-ref tool (string-append "organization" ""))) (lambda args 'null)))
                (mod (catch #t (lambda () (njson-ref tool (string-append "module" ""))) (lambda args 'null))))
            (and (not (or (eq? org 'null) (and (njson? org) (njson-null? org)) (and (string? org) (string-null? org))))
              (not (or (eq? mod 'null) (and (njson? mod) (njson-null? mod)) (and (string? mod) (string-null? mod))))
              (> (string-length (if (string? org) org "")) 0)
              (> (string-length (if (string? mod) mod "")) 0)
            ) ;and
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (display-version)
      "Display version information"
      (display "Goldfish Scheme ")
      (display (version))
      (display " by LiiiLabs")
      (newline)
    ) ;define

    (define (display-command-line cmd desc . extra-lines)
      "Display a command with its description, aligned to column 19"
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
      "Display dynamic commands from gfproject.scm with one-line descriptions"
      (let ((tool-names (list-sort string<? (njson-keys tools))))
        (for-each (lambda (tool-name)
                    (let ((desc (get-tool-description tools (string-append tool-name "") "en_US")))
                      (display-command-line tool-name desc)
                    ) ;let
                  ) ;lambda
          tool-names
        ) ;for-each
      ) ;let
    ) ;define

    (define (display-help)
      "Display help information matching the C++ display_help() format"
      (let* ((config (load-gfproject)) (tools (njson-ref config (string-append "tools" ""))))
        (display-version)
        (newline)
        (display "Commands:")
        (newline)
        (let ((help-desc (get-tool-description tools "help" "en_US")))
          (display-command-line "help" help-desc)
        ) ;let
        (if (not (njson-empty? tools))
          (let ((other-tool-names (filter (lambda (name) (not (string=? name "help"))) (njson-keys tools))
                ) ;other-tool-names
               ) ;
            (for-each (lambda (tool-name)
                        (let ((desc (get-tool-description tools (string-append tool-name "") "en_US")))
                          (display-command-line tool-name desc)
                        ) ;let
                      ) ;lambda
              (list-sort string<? other-tool-names)
            ) ;for-each
          ) ;let
        ) ;if
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
      "Search for README.md in tools/<tool-name>/ directory"
      (let ((cwd (getcwd)))
        (if cwd
          (let ((readme-path (path->string (path-join (path cwd) (path "tools") (path tool-name) (path "README.md"))
                             ) ;path->string
                ) ;readme-path
               ) ;
            (if (file-exists? readme-path) readme-path #f)
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (display-tool-help tool-name)
      "Display detailed help for a specific tool"
      (let* ((config (load-gfproject))
             (tools (njson-ref config (string-append "tools" "")))
             (tool (catch #t (lambda () (njson-ref tools (string-append tool-name ""))) (lambda args 'null)))
            ) ;
        (if (or (eq? tool 'null) (and (njson? tool) (njson-null? tool)))
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
      "Main entry point for help command"
      (let ((parser (make-help-arg-parser)))
        (parser :parse-argv (command-line))
        (let ((positionals (parser :positionals)))
          (if (null? positionals) (display-help) (display-tool-help (car positionals)))
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
