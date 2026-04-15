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
    (liii json)
    (liii string)
    (liii path)
    (liii os)
    (liii error)
    (liii sys)
    (liii list)
  ) ;import
  (export main
    load-gfproject
    get-tool-description
    display-help
  ) ;export
  (begin

    (define (load-gfproject)
      "Load merged gfproject.json via C++ glue"
      (string->json (g_gfproject-load-config))
    ) ;define

    (define (get-tool-description tools
              tool-name
              lang
            ) ;get-tool-description
      "Get description for a tool in specified language"
      (let* ((tool (json-ref tools tool-name))
             (desc (if (json-null? tool)
                     (json-null)
                     (json-ref tool "description")
                   ) ;if
             ) ;desc
            ) ;
        (if (json-null? desc)
          ""
          (let ((lang-desc (json-ref desc lang)))
            (if (json-null? lang-desc)
              (let ((en-desc (json-ref desc "en_US")))
                (if (json-null? en-desc) "" en-desc)
              ) ;let
              lang-desc
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (json-empty? x)
      "Check if json-ref returned empty result (null or empty list)"
      (or (json-null? x)
        (and (list? x) (null? x))
      ) ;or
    ) ;define

    (define (has-tool-implementation? tools
              tool-name
            ) ;has-tool-implementation?
      "Check if a tool has Scheme implementation (has organization and module)"
      (let ((tool (json-ref tools tool-name)))
        (if (json-empty? tool)
          #f
          (let ((org (json-ref tool "organization"))
                (mod (json-ref tool "module"))
               ) ;
            (and (not (json-empty? org))
              (not (json-empty? mod))
              (> (string-length org) 0)
              (> (string-length mod) 0)
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

    (define (display-command-line
              cmd
              desc
              .
              extra-lines
            ) ;
      "Display a command with its description, aligned to column 19"
      (display "  ")
      (display cmd)
      (let ((pad (- 19
                   (+ (string-length "  ")
                     (string-length cmd)
                   ) ;+
                 ) ;-
            ) ;pad
           ) ;
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
      (for-each (lambda (line)
                  (display (make-string 19 #\space))
                  (display line)
                  (newline)
                ) ;lambda
        extra-lines
      ) ;for-each
    ) ;define

    (define (display-dynamic-commands tools)
      "Display dynamic commands from gfproject.json with one-line descriptions"
      (let ((tool-names (list-sort string<? (json-keys tools))
            ) ;tool-names
           ) ;
        (for-each (lambda (tool-name)
                    (let ((desc (get-tool-description tools
                                  tool-name
                                  "en_US"
                                ) ;get-tool-description
                          ) ;desc
                         ) ;
                      (display-command-line tool-name desc)
                    ) ;let
                  ) ;lambda
          tool-names
        ) ;for-each
      ) ;let
    ) ;define

    (define (display-help)
      "Display help information matching the C++ display_help() format"
      (let* ((config (load-gfproject))
             (tools (json-ref config "tools"))
            ) ;
        (display-version)
        (newline)
        (display "Commands:")
        (newline)
        (let ((help-desc (get-tool-description tools
                           "help"
                           "en_US"
                         ) ;get-tool-description
              ) ;help-desc
             ) ;
          (display-command-line "help" help-desc)
        ) ;let
        (if (not (json-null? tools))
          (let ((other-tool-names (filter (lambda (name)
                                            (not (string=? name "help"))
                                          ) ;lambda
                                    (json-keys tools)
                                  ) ;filter
                ) ;other-tool-names
               ) ;
            (for-each (lambda (tool-name)
                        (let ((desc (get-tool-description tools
                                      tool-name
                                      "en_US"
                                    ) ;get-tool-description
                              ) ;desc
                             ) ;
                          (display-command-line tool-name desc)
                        ) ;let
                      ) ;lambda
              (list-sort string<? other-tool-names)
            ) ;for-each
          ) ;let
        ) ;if
        (display-command-line "FILE"
          "Load and evaluate Scheme code from FILE"
        ) ;display-command-line
        (newline)
        (display "Options:")
        (newline)
        (display-command-line "--mode, -m MODE"
          "Set mode: default, liii, sicp, r7rs, s7"
        ) ;display-command-line
        (display-command-line "-I DIR"
          "Prepend DIR to library search path"
        ) ;display-command-line
        (display-command-line "-A DIR"
          "Append DIR to library search path"
        ) ;display-command-line
        (newline)
        (display "Type 'gf help <command>' for more information on a specific command."
        ) ;display
        (newline)
      ) ;let*
    ) ;define

    (define (find-tool-readme tool-name)
      "Search for README.md in tools/<tool-name>/ directory"
      (let ((cwd (getcwd)))
        (if cwd
          (let ((readme-path (path->string (path-join (path cwd)
                                             (path "tools")
                                             (path tool-name)
                                             (path "README.md")
                                           ) ;path-join
                             ) ;path->string
                ) ;readme-path
               ) ;
            (if (file-exists? readme-path)
              readme-path
              #f
            ) ;if
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (display-tool-help tool-name)
      "Display detailed help for a specific tool"
      (let* ((config (load-gfproject))
             (tools (json-ref config "tools"))
             (tool (json-ref tools tool-name))
            ) ;
        (if (json-null? tool)
          (begin
            (display "Unknown command: ")
            (display tool-name)
            (newline)
          ) ;begin
          (let ((readme-path (find-tool-readme tool-name)
                ) ;readme-path
               ) ;
            (if readme-path
              (begin
                (display (path-read-text readme-path))
                (newline)
              ) ;begin
              (let ((cmd (string-append "gf "
                           tool-name
                           " --help"
                         ) ;string-append
                    ) ;cmd
                   ) ;
                (os-call cmd)
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (main)
      "Main entry point for help command"
      (let ((args (command-line)))
        (if (> (length args) 2)
          (display-tool-help (list-ref args 2))
          (display-help)
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
