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

;; 从当前目录向上定位项目配置，使 gf test 进入 domain 目录后仍能读取
;; 项目根部的 gfproject.json，并按 depends-on 构造完整的库搜索路径。
(define-library (liii goldtest-project)
  (import (scheme base)
    (liii json)
    (liii os)
    (liii path)
    (liii string)
    (liii gfproject config)
  ) ;import
  (export %find-project-root %project-domains-list %project-domain-search-roots
    %domain-roots %find-domain-for-test-file %goldfish-cmd-flags
  ) ;export
  (begin

    (define (%config-path dir)
      (path->string (path-join dir "gfproject.json"))
    ) ;define

    (define (%find-project-root dir)
      (let ((parent (path->string (path-parent dir))))
        (cond ((path-file? (%config-path dir)) dir)
              ((string=? parent dir) #f)
              (else (%find-project-root parent))
        ) ;cond
      ) ;let
    ) ;define

    (define (%domain-tests-dir root)
      (cond ((or (string=? root "") (string=? root ".")) "tests")
            ((string-ends? root "/") (string-append root "tests"))
            (else (string-append root "/tests"))
      ) ;cond
    ) ;define

    (define (%json-domain-deps d)
      (if (and (json-object? d) (json-contains-key? d "depends-on"))
        (let ((deps (json-ref d "depends-on")))
          (if (json-array? deps) (vector->list deps) '())
        ) ;let
        '()
      ) ;if
    ) ;define

    (define (%json-domain-mode d)
      (if (and (json-object? d) (json-contains-key? d "mode"))
        (json-ref d "mode")
        "r7rs"
      ) ;if
    ) ;define

    (define (%json-domain->domain name d)
      (let ((root (if (and (json-object? d) (json-contains-key? d "root")) (json-ref d "root") "")
            ) ;root
           ) ;
        (list (cons 'name name)
          (cons 'root root)
          (cons 'depends-on (%json-domain-deps d))
          (cons 'mode (%json-domain-mode d))
          (cons 'tests (%domain-tests-dir root))
        ) ;list
      ) ;let
    ) ;define

    (define (%read-project-domains project-root)
      (if (not project-root)
        '()
        (let* ((config (gfproject-read-file (%config-path project-root)))
               (domains (gfproject-extract-domains config))
              ) ;
          (if (equal? domains '(()))
            '()
            (map (lambda (name) (%json-domain->domain name (json-ref domains name)))
              (json-keys domains)
            ) ;map
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (%project-domains-list)
      (let ((domains (%read-project-domains (%find-project-root (getcwd)))))
        (if (null? domains) (gfproject-domains-list) domains)
      ) ;let
    ) ;define

    (define (%project-domain-search-roots)
      (let* ((project-root (%find-project-root (getcwd)))
             (roots
               (map
                 (lambda (d) (string-trim-right (cdr (assq 'root d)) #\/))
                 (%project-domains-list)
               ) ;map
             ) ;roots
            ) ;
        (if project-root
          (append roots
            (map (lambda (root) (path->string (path-join project-root root))) roots)
          ) ;append
          roots
        ) ;if
      ) ;let*
    ) ;define

    (define (%find-domain-for-test-file test-file)
      (let ((cwd (string-replace (string-trim-right (getcwd) #\/) "\\" "/")))
        (let loop
          ((ds (%project-domains-list)))
          (if (null? ds)
            #f
            (let* ((d (car ds))
                   (root (string-trim-right (cdr (assq 'root d)) #\/))
                   (prefix (string-append (cdr (assq 'tests d)) "/"))
                  ) ;
              (if
                (or (string-starts? test-file prefix)
                  (and (string-starts? test-file "tests/")
                    (> (string-length root) 0)
                    (or (string-ends? cwd (string-append "/" root)) (string=? cwd root))
                  ) ;and
                ) ;or
                d
                (loop (cdr ds))
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (%domain-named name domains)
      (let loop
        ((rest domains))
        (cond ((null? rest) #f)
              ((string=? name (cdr (assq 'name (car rest)))) (car rest))
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 当前 domain 在前，depends-on 按深度优先展开；seen 同时去重并防环。
    (define (%domain-roots domain domains)
      (let loop
        ((names (cons (cdr (assq 'name domain)) (cdr (assq 'depends-on domain))))
         (seen '())
         (roots '())
        ) ;
        (if (null? names)
          (reverse roots)
          (let* ((name (car names))
                 (d (and (not (member name seen string=?)) (%domain-named name domains)))
                ) ;
            (if (not d)
              (loop (cdr names) seen roots)
              (let ((root (cdr (assq 'root d))))
                (loop (append (cdr (assq 'depends-on d)) (cdr names))
                  (cons name seen)
                  (if (member root roots string=?) roots (cons root roots))
                ) ;loop
              ) ;let
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 这些域根已内置在 *load-path*，运行其测试无需 -I 追加搜索路径。
    (define %builtin-domain-roots
      '("" "goldfish/" "r7rs/" "library/" "bignum/" "cmark/" "tui/")
    ) ;define

    (define (%include-root project-root root test-file)
      (let ((clean-root (string-trim-right root #\/)))
        (if (and project-root (string-starts? test-file "tests/"))
          (path->string (path-join project-root clean-root))
          clean-root
        ) ;if
      ) ;let
    ) ;define

    (define (%include-flag project-root root test-file)
      (if (member root %builtin-domain-roots string=?)
        ""
        (string-append "-I " (%include-root project-root root test-file) " ")
      ) ;if
    ) ;define

    (define (%goldfish-cmd-flags test-file . maybe-domain)
      (let* ((project-root (%find-project-root (getcwd)))
             (domains (%project-domains-list))
             (d (if (pair? maybe-domain)
                  (car maybe-domain)
                  (and test-file (%find-domain-for-test-file test-file))
                ) ;if
             ) ;d
             (m (and d (assq 'mode d)))
             (mode
               (if (and m (string? (cdr m))) (cdr m) "r7rs")
             ) ;mode
             (roots (if d (%domain-roots d domains) '()))
             (inc
               (apply string-append
                 (map (lambda (root) (%include-flag project-root root test-file)) roots)
               ) ;apply
             ) ;inc
            ) ;
        (string-append " -m " mode " " inc)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
