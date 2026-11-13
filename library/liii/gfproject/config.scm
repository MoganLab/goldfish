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
  (export gfproject-root gfproject-get-gf-lib gfproject-find-lib-path
    gfproject-find-local-path gfproject-extract-tools gfproject-deep-merge
    gfproject-load-config-bundle gfproject-load-config gfproject-read-file
    gfproject--opt-gf-lib bundle-ref gfproject-extract-domains gfproject-domains
    gfproject-domains-list gfproject-hint-dirs
  ) ;export
  (begin

    (define gfproject-root g_project-root)

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
      (let ((entry (assoc key bundle)))
        (and entry (cdr entry))
      ) ;let
    ) ;define

    (define (gfproject-find-local-path)
      (let ((root (gfproject-root)))
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

    (define (gfproject-extract-domains config)
      (if (and (json-object? config) (json-contains-key? config "domains"))
        (let ((domains (json-ref config "domains")))
          (if (json-object? domains) domains '(()))
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
             (lib-domains (gfproject-extract-domains lib-config))
             (local-domains (gfproject-extract-domains local-config))
             (merged-domains (gfproject-deep-merge lib-domains local-domains))
             (merged-config
               (let* ((c1 (gfproject-deep-merge lib-config local-config))
                      (c2 (if (equal? merged-tools '(()))
                            c1
                            (if (json-contains-key? c1 "tools")
                              (json-set c1 "tools" merged-tools)
                              (json-push c1 "tools" merged-tools)
                            ) ;if
                          ) ;if
                      ) ;c2
                      (c3 (if (equal? merged-domains '(()))
                            c2
                            (if (json-contains-key? c2 "domains")
                              (json-set c2 "domains" merged-domains)
                              (json-push c2 "domains" merged-domains)
                            ) ;if
                          ) ;if
                      ) ;c3
                     ) ;
                 c3
               ) ;let*
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
          (cons "lib_domains" lib-domains)
          (cons "local_domains" local-domains)
          (cons "merged_domains" merged-domains)
        ) ;list
      ) ;let*
    ) ;define

    (define (gfproject-load-config . opt-gf-lib)
      (bundle-ref (apply gfproject-load-config-bundle opt-gf-lib) "merged_config")
    ) ;define

    (define (gfproject-domains . opt-gf-lib)
      (let* ((bundle (apply gfproject-load-config-bundle opt-gf-lib))
             (merged-domains (bundle-ref bundle "merged_domains"))
            ) ;
        (if (or (not merged-domains) (equal? merged-domains '(()))) '() merged-domains)
      ) ;let*
    ) ;define

    (define (%domain-tests-dir root)
      (cond ((or (string=? root "") (string=? root ".")) "tests")
            ((string-ends? root "/") (string-append root "tests"))
            (else (string-append root "/tests"))
      ) ;cond
    ) ;define

    ;; domain 内的类型接口（hint）目录：与 tests 目录同源，
    ;; 由 domain root 推导，板块内库与 hint 库一一对应。
    (define (%domain-hint-dir root)
      (cond ((or (string=? root "") (string=? root ".")) "hint")
            ((string-ends? root "/") (string-append root "hint"))
            (else (string-append root "/hint"))
      ) ;cond
    ) ;define

    (define (%domain-mode-of d)
      (if (and (json-object? d) (json-contains-key? d "mode"))
        (let ((m (json-ref d "mode")))
          (if (and (string? m) (> (string-length m) 0)) m "r7rs")
        ) ;let
        "r7rs"
      ) ;if
    ) ;define

    (define (%domain-deps-of d)
      (if (and (json-object? d) (json-contains-key? d "depends-on"))
        (let ((v (json-ref d "depends-on")))
          (if (json-array? v) (vector->list v) '())
        ) ;let
        '()
      ) ;if
    ) ;define

    (define (gfproject-domains-list . opt-gf-lib)
      (let ((d-obj (apply gfproject-domains opt-gf-lib)))
        (if (or (null? d-obj) (equal? d-obj '(())))
          (list (list (cons 'name "default")
                  (cons 'root "")
                  (cons 'depends-on '())
                  (cons 'mode "r7rs")
                  (cons 'tests "tests")
                  (cons 'hint "hint")
                ) ;list
          ) ;list
          (map
            (lambda (name)
              (let* ((d (json-ref d-obj name))
                     (root (if (and (json-object? d) (json-contains-key? d "root")) (json-ref d "root") "")
                     ) ;root
                    ) ;
                (list (cons 'name name)
                  (cons 'root root)
                  (cons 'depends-on (%domain-deps-of d))
                  (cons 'mode (%domain-mode-of d))
                  (cons 'tests (%domain-tests-dir root))
                  (cons 'hint (%domain-hint-dir root))
                ) ;list
              ) ;let*
            ) ;lambda
            (json-keys d-obj)
          ) ;map
        ) ;if
      ) ;let
    ) ;define

    ;; 各 domain 的类型接口（hint）目录列表：与 tests 目录一样由 domain 推导
    (define (gfproject-hint-dirs . opt-gf-lib)
      (map (lambda (d) (bundle-ref d 'hint))
        (apply gfproject-domains-list opt-gf-lib)
      ) ;map
    ) ;define

  ) ;begin
) ;define-library
