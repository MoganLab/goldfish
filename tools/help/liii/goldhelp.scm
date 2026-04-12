;
; Copyright (C) 2026 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

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

    (define (find-gfproject-path kind)
      "Search for gfproject.json - 'local or 'lib"
      (let ((cwd (getcwd)))
        (cond
         ((eq? kind 'local)
          (if cwd
              (let ((test-path (path->string (path-join (path cwd) (path "gfproject.json")))))
                (if (file-exists? test-path)
                    test-path
                    #f))
              #f))
         ((eq? kind 'lib)
          ;; First try GF_LIB environment variable
          (let ((gf-lib (getenv "GF_LIB")))
            (if gf-lib
                (let ((lib-path (path->string (path-join (path gf-lib) (path "gfproject.json")))))
                  (if (file-exists? lib-path)
                      lib-path
                      #f))
                ;; Fallback: search from executable location upward
                (let ((exe-path (executable)))
                  (if exe-path
                      (let search ((dir (path-dirname (path exe-path))))
                        (if dir
                            (let ((gfproject-path (path->string (path-join dir (path "gfproject.json")))))
                              (if (file-exists? gfproject-path)
                                  gfproject-path
                                  (search (path-dirname dir))))
                            #f))
                      #f)))))
         (else #f))))

    (define (json-merge-tools lib-tools local-tools)
      "Merge two tools JSON objects, local takes precedence (except 'help')"
      (cond
       ((json-null? lib-tools)
        (if (json-null? local-tools)
            (make-json)
            local-tools))
       ((json-null? local-tools)
        lib-tools)
       (else
        ;; Start with lib tools as base, then add local tools (except "help")
        (let ((merged (make-json)))
          ;; Add lib tools (except "help")
          (for-each
           (lambda (key)
             (when (not (string=? key "help"))
               (json-set! merged key (json-ref lib-tools key))))
           (json-keys lib-tools))
          ;; Add local tools (except "help"), overwriting lib values
          (for-each
           (lambda (key)
             (when (not (string=? key "help"))
               (json-set! merged key (json-ref local-tools key))))
           (json-keys local-tools))
          merged)))))

    (define (load-gfproject)
      "Load and merge gfproject.json from local and lib, local takes precedence"
      (let ((local-path (find-gfproject-path 'local))
            (lib-path (find-gfproject-path 'lib)))
        (let ((lib-config (if lib-path
                              (let ((content (path-read-text lib-path)))
                                (string->json content))
                              (make-json)))
              (local-config (if local-path
                                (let ((content (path-read-text local-path)))
                                  (string->json content))
                                (make-json))))
          (let ((lib-tools (json-ref lib-config "tools"))
                (local-tools (json-ref local-config "tools")))
            (json-merge-tools lib-tools local-tools)))))

    (define (get-tool-description tools tool-name lang)
      "Get description for a tool in specified language"
      (let* ((tool (json-ref tools tool-name))
             (desc (if (json-null? tool)
                       (json-null)
                       (json-ref tool "description"))))
        (if (json-null? desc)
            ""
            (let ((lang-desc (json-ref desc lang)))
              (if (json-null? lang-desc)
                  (let ((en-desc (json-ref desc "en_US")))
                    (if (json-null? en-desc)
                        ""
                        en-desc))
                  lang-desc)))))

    (define (json-empty? x)
      "Check if json-ref returned empty result (null or empty list)"
      (or (json-null? x)
          (and (list? x) (null? x))))

    (define (has-tool-implementation? tools tool-name)
      "Check if a tool has Scheme implementation (has organization and module)"
      (let ((tool (json-ref tools tool-name)))
        (if (json-empty? tool)
            #f
            (let ((org (json-ref tool "organization"))
                  (mod (json-ref tool "module")))
              (and (not (json-empty? org))
                   (not (json-empty? mod))
                   (> (string-length org) 0)
                   (> (string-length mod) 0))))))

    (define (display-version)
      "Display version information"
      (display "Goldfish Scheme ")
      (display (version))
      (display " by LiiiLabs")
      (newline))

    (define (display-command-line cmd desc . extra-lines)
      "Display a command with its description, aligned to column 19"
      (display "  ")
      (display cmd)
      (let ((pad (- 19 (+ (string-length "  ") (string-length cmd)))))
        (if (> pad 0)
            (display (make-string pad #\space))
            (begin
              (newline)
              (display (make-string 19 #\space)))))
      (display desc)
      (newline)
      ; Display extra continuation lines if any
      (for-each
       (lambda (line)
         (display (make-string 19 #\space))
         (display line)
         (newline))
       extra-lines))

    (define (display-dynamic-commands tools)
      "Display dynamic commands from gfproject.json with one-line descriptions"
      (let ((tool-names (list-sort string<? (json-keys tools))))
        (for-each
         (lambda (tool-name)
           (let ((desc (get-tool-description tools tool-name "en_US")))
             (display-command-line tool-name desc)))
         tool-names)))

    (define (display-help)
      "Display help information matching the C++ display_help() format"
      (let* ((config (load-gfproject))
             (tools (json-ref config "tools")))
        (display-version)
        (newline)
        (display "Commands:")
        (newline)
        ; Display help first as special command
        (let ((help-desc (get-tool-description tools "help" "en_US")))
          (display-command-line "help" help-desc))
        ; Display other commands in alphabetical order
        (if (not (json-null? tools))
            (let ((other-tool-names
                   (filter (lambda (name) (not (string=? name "help")))
                           (json-keys tools))))
              (for-each
               (lambda (tool-name)
                 (let ((desc (get-tool-description tools tool-name "en_US")))
                   (display-command-line tool-name desc)))
               (list-sort string<? other-tool-names))))
        ; FILE pseudo-command for loading Scheme files
        (display-command-line "FILE" "Load and evaluate Scheme code from FILE")
        (newline)
        (display "Options:")
        (newline)
        (display-command-line "--mode, -m MODE" "Set mode: default, liii, sicp, r7rs, s7")
        (display-command-line "-I DIR" "Prepend DIR to library search path")
        (display-command-line "-A DIR" "Append DIR to library search path")
        (newline)
        (display "Type 'gf help <command>' for more information on a specific command.")
        (newline)))

    (define (find-tool-readme tool-name)
      "Search for README.md in tools/<tool-name>/ directory"
      (let ((cwd (getcwd)))
        (if cwd
            (let ((readme-path (path->string (path-join (path cwd) (path "tools") (path tool-name) (path "README.md")))))
              (if (file-exists? readme-path)
                  readme-path
                  #f))
            #f)))

    (define (display-tool-help tool-name)
      "Display detailed help for a specific tool"
      (let* ((config (load-gfproject))
             (tools (json-ref config "tools"))
             (tool (json-ref tools tool-name)))
        (if (json-null? tool)
            (begin
              (display "Unknown command: ")
              (display tool-name)
              (newline))
            (let ((readme-path (find-tool-readme tool-name)))
              (if readme-path
                  (begin
                    (display (path-read-text readme-path))
                    (newline))
                  (let ((cmd (string-append "gf " tool-name " --help")))
                    (os-call cmd)))))))

    (define (main)
      "Main entry point for help command"
      (let ((args (command-line)))
        (if (> (length args) 2)
            (display-tool-help (list-ref args 2))
            (display-help))))

  ) ;begin
) ;define-library
