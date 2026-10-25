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
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii gfproject config)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii json)
    (liii list)
    (liii os)
    (liii path)
    (liii string)
  ) ;import
  (export gfproject-get-gf-lib gfproject-find-lib-path gfproject-find-local-path
    gfproject-extract-tools gfproject-deep-merge gfproject-load-config-bundle
    gfproject-load-config gfproject-read-file gfproject--opt-gf-lib bundle-ref
  ) ;export
  (begin

    (define (gfproject-get-gf-lib)
      (g_goldfish-library-dir)
    ) ;define

    (define (gfproject--opt-gf-lib opt-gf-lib)
      (if (and (pair? opt-gf-lib) (string? (car opt-gf-lib)))
        (car opt-gf-lib)
        (gfproject-get-gf-lib)
      ) ;if
    ) ;define

    (define (bundle-ref bundle key)
      (cdr (assoc key bundle))
    ) ;define

    (define (gfproject-find-local-path)
      (let ((root (g_project-root)))
        (and root (path->string (path-join root "gfproject.json")))
      ) ;let
    ) ;define

    (define (gfproject-find-lib-path . opt-gf-lib)
      (let* ((gf-lib (gfproject--opt-gf-lib opt-gf-lib))
             (p1 (path-join gf-lib "gfproject.json"))
             (p2 (path-join (path-parent gf-lib) "gfproject.json"))
            ) ;
        (cond ((path-file? p1) (path->string p1))
              ((path-file? p2) (path->string p2))
              (else #f)
        ) ;cond
      ) ;let*
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
             (merged-config
               (if (equal? lib-config '(()))
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
          (cons "lib_path" lib-path)
          (cons "local_path" local-path)
          (cons "merged_config" merged-config)
          (cons "lib_tools" lib-tools)
          (cons "local_tools" local-tools)
          (cons "merged_tools" merged-tools)
        ) ;list
      ) ;let*
    ) ;define

    (define (gfproject-load-config . opt-gf-lib)
      (bundle-ref (apply gfproject-load-config-bundle opt-gf-lib) "merged_config")
    ) ;define

  ) ;begin
) ;define-library
