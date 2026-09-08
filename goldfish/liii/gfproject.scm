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

(define-library (liii gfproject)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii json)
    (liii os)
    (liii path)
  ) ;import
  (export gfproject-get-gf-lib gfproject-find-lib-path gfproject-find-local-path
    gfproject-read-file gfproject-extract-tools gfproject-deep-merge
    gfproject-load-config-bundle gfproject-load-config
    gfproject-load-config-string gfproject-find-tool-root gfproject-resolve-tool
    gfproject-resolve-tool-bundle gfproject-prepare-and-run-tool
    gfproject-run-tool g_gfproject-load-config
  ) ;export

  (begin

    (define (gfproject-get-gf-lib)
      (cond ((and (defined? 'rootlet) (defined? 'g_goldfish-library-dir (rootlet)))
             (((rootlet) 'g_goldfish-library-dir))
            ) ;
            ((defined? 'g_goldfish-library-dir) (g_goldfish-library-dir))
            (else #f)
      ) ;cond
    ) ;define

    (define (gfproject-find-local-path)
      (let ((p (path-join (getcwd) "gfproject.json")))
        (if (path-file? p) (path->string p) #f)
      ) ;let
    ) ;define

    (define (gfproject-find-lib-path . opt-gf-lib)
      (let ((gf-lib (if (and (pair? opt-gf-lib) (string? (car opt-gf-lib)))
                      (car opt-gf-lib)
                      (gfproject-get-gf-lib)
                    ) ;if
            ) ;gf-lib
           ) ;
        (if gf-lib
          (let ((p1 (path-join gf-lib "gfproject.json"))
                (p2 (path-join (path-parent gf-lib) "gfproject.json"))
               ) ;
            (cond ((path-file? p1) (path->string p1))
                  ((path-file? p2) (path->string p2))
                  (else #f)
            ) ;cond
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (gfproject-read-file path)
      (if (or (not path) (not (path-file? path)))
        '(())
        (catch #t
          (lambda ()
            (let* ((text (path-read-text path)) (data (string->json text)))
              (if (json-object? data) data '(()))
            ) ;let*
          ) ;lambda
          (lambda (type info) '(()))
        ) ;catch
      ) ;if
    ) ;define

    (define (gfproject-extract-tools config)
      (if (and (json-object? config) (json-contains-key? config "tools"))
        (let ((tools (json-ref config "tools")))
          (if (json-object? tools) tools '(()))
        ) ;let
        '(())
      ) ;if
    ) ;define

    (define (gfproject-deep-merge base overlay)
      (cond ((equal? base '(())) overlay)
            ((equal? overlay '(())) base)
            ((and (json-object? base) (json-object? overlay))
             (let loop
               ((keys (json-keys overlay)) (acc base))
               (if (null? keys)
                 acc
                 (let* ((k (car keys)) (v-overlay (json-ref overlay k)))
                   (if (json-contains-key? acc k)
                     (loop (cdr keys)
                       (json-set acc k (gfproject-deep-merge (json-ref acc k) v-overlay))
                     ) ;loop
                     (loop (cdr keys) (json-push acc k v-overlay))
                   ) ;if
                 ) ;let*
               ) ;if
             ) ;let
            ) ;
            (else overlay)
      ) ;cond
    ) ;define

    (define (gfproject-load-config-bundle . opt-gf-lib)
      (let* ((lib-path (apply gfproject-find-lib-path opt-gf-lib))
             (local-path (gfproject-find-local-path))
             (lib-config (gfproject-read-file lib-path))
             (local-config (gfproject-read-file local-path))
             (lib-tools (gfproject-extract-tools lib-config))
             (local-tools (gfproject-extract-tools local-config))
             (merged-tools (gfproject-deep-merge lib-tools local-tools))
             (merged-config (if (equal? lib-config '(()))
                              (if (equal? merged-tools '(())) '(()) (list (cons "tools" merged-tools)))
                              (if (json-contains-key? lib-config "tools")
                                (json-set lib-config "tools" merged-tools)
                                (json-push lib-config "tools" merged-tools)
                              ) ;if
                            ) ;if
             ) ;merged-config
            ) ;
        (list (cons "lib_config" lib-config)
          (cons "local_config" local-config)
          (cons "merged_config" merged-config)
        ) ;list
      ) ;let*
    ) ;define

    (define (gfproject-load-config . opt-gf-lib)
      (let ((bundle (apply gfproject-load-config-bundle opt-gf-lib)))
        (cdr (assoc "merged_config" bundle))
      ) ;let
    ) ;define

    (define (gfproject-load-config-string . opt-gf-lib)
      (json->string (apply gfproject-load-config opt-gf-lib))
    ) ;define

    (define g_gfproject-load-config gfproject-load-config-string)

    (define (gfproject-find-tool-root command . opt-gf-lib)
      (let* ((gf-lib (if (and (pair? opt-gf-lib) (string? (car opt-gf-lib)))
                       (car opt-gf-lib)
                       (gfproject-get-gf-lib)
                     ) ;if
             ) ;gf-lib
             (candidates (list (path-join (getcwd) "tools" command)
                           (if gf-lib (path-join gf-lib "tools" command) #f)
                           (if gf-lib (path-join (path-parent gf-lib) "tools" command) #f)
                         ) ;list
             ) ;candidates
            ) ;
        (let loop
          ((cs candidates))
          (cond ((null? cs) #f)
                ((not (car cs)) (loop (cdr cs)))
                ((path-dir? (car cs)) (path->string (car cs)))
                (else (loop (cdr cs)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    (define (gfproject-resolve-tool command . opt-gf-lib)
      (let* ((bundle (apply gfproject-load-config-bundle opt-gf-lib))
             (merged-config (cdr (assoc "merged_config" bundle)))
             (merged-tools (gfproject-extract-tools merged-config))
             (has-merged? (json-contains-key? merged-tools command))
            ) ;
        (if (not has-merged?) #f (json-ref merged-tools command))
      ) ;let*
    ) ;define

    (define (gfproject-resolve-tool-bundle command . opt-gf-lib)
      (let* ((bundle (apply gfproject-load-config-bundle opt-gf-lib))
             (local-config (cdr (assoc "local_config" bundle)))
             (lib-config (cdr (assoc "lib_config" bundle)))
             (merged-config (cdr (assoc "merged_config" bundle)))
             (local-tools (gfproject-extract-tools local-config))
             (lib-tools (gfproject-extract-tools lib-config))
             (merged-tools (gfproject-extract-tools merged-config))
             (has-local? (json-contains-key? local-tools command))
             (has-lib? (json-contains-key? lib-tools command))
             (has-merged? (json-contains-key? merged-tools command))
            ) ;
        (if (not has-merged?)
          #f
          (list (cons "has-local-override" has-local?)
            (cons "has-lib-tool" has-lib?)
            (cons "has-merged-tool" has-merged?)
            (cons "merged-tool" (json-ref merged-tools command))
            (cons "lib-tool" (if has-lib? (json-ref lib-tools command) '(())))
          ) ;list
        ) ;if
      ) ;let*
    ) ;define

    (define (gfproject-prepare-and-run-tool command tool-config gf-lib allow-fallback)
      (if (not (json-object? tool-config))
        (if allow-fallback
          #f
          (begin
            (display (string-append "Error: Tool '" command "' config must be a JSON object.\n")
              (current-error-port)
            ) ;display
            1
          ) ;begin
        ) ;if
        (let ((org (json-ref tool-config "organization"))
              (module (json-ref tool-config "module"))
             ) ;
          (if (or (not (string? org)) (not (string? module)))
            (if allow-fallback
              #f
              (begin
                (display (string-append "Error: Tool '"
                           command
                           "' is not fully implemented (missing organization or module).\n"
                         ) ;string-append
                  (current-error-port)
                ) ;display
                1
              ) ;begin
            ) ;if
            (let ((tool-root (gfproject-find-tool-root command gf-lib)))
              (if (not tool-root)
                (if allow-fallback
                  #f
                  (begin
                    (display (string-append "Error: tools/" command "/" org " directory not found.\n")
                      (current-error-port)
                    ) ;display
                    1
                  ) ;begin
                ) ;if
                (begin
                  (set! *load-path* (cons tool-root *load-path*))
                  (let* ((import-ok #t)
                         (import-err "")
                         (_ (catch #t
                              (lambda ()
                                (eval `(import (,(string->symbol org)
                                                ,(string->symbol module))) (rootlet))
                              ) ;lambda
                              (lambda (tag info)
                                (set! import-ok #f)
                                (set! import-err (if (pair? info) (car info) "import failed"))
                              ) ;lambda
                            ) ;catch
                         ) ;_
                        ) ;
                    (if (not import-ok)
                      (if allow-fallback
                        #f
                        (begin
                          (display (string-append "Error importing (" org " " module "):\n")
                            (current-error-port)
                          ) ;display
                          (display import-err (current-error-port))
                          (newline (current-error-port))
                          1
                        ) ;begin
                      ) ;if
                      (let ((main-proc (catch #t (lambda () (eval 'main (rootlet))) (lambda (tag info) #f)))
                           ) ;
                        (if (not (procedure? main-proc))
                          (if allow-fallback
                            #f
                            (begin
                              (display (string-append "Error: Failed to find main function in (" org " " module ").\n")
                                (current-error-port)
                              ) ;display
                              1
                            ) ;begin
                          ) ;if
                          (let ((res (catch #t
                                       (lambda () (main-proc))
                                       (lambda (tag info) (display (format #f "~A\n" info) (current-error-port)) 1)
                                     ) ;catch
                                ) ;res
                               ) ;
                            (if (integer? res) res 0)
                          ) ;let
                        ) ;if
                      ) ;let
                    ) ;if
                  ) ;let*
                ) ;begin
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (gfproject-run-tool command . opt-gf-lib)
      (let ((bundle (apply gfproject-resolve-tool-bundle command opt-gf-lib)))
        (if (not bundle)
          #f
          (let* ((gf-lib (if (and (pair? opt-gf-lib) (string? (car opt-gf-lib)))
                           (car opt-gf-lib)
                           (gfproject-get-gf-lib)
                         ) ;if
                 ) ;gf-lib
                 (has-local? (cdr (assoc "has-local-override" bundle)))
                 (has-lib? (cdr (assoc "has-lib-tool" bundle)))
                 (merged-tool (cdr (assoc "merged-tool" bundle)))
                 (lib-tool (cdr (assoc "lib-tool" bundle)))
                 (builtin-fallback? (member command '("help" "version" "eval"
                                                      "load" "repl" "run"))
                 ) ;builtin-fallback?
                ) ;
            (if (and has-local? has-lib?)
              (let ((ret (gfproject-prepare-and-run-tool command merged-tool gf-lib #t)))
                (if ret
                  ret
                  (gfproject-prepare-and-run-tool command lib-tool gf-lib builtin-fallback?)
                ) ;if
              ) ;let
              (gfproject-prepare-and-run-tool command merged-tool gf-lib builtin-fallback?)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
