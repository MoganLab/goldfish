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

(define-library (liii pi compass entry)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii sort)
    (liii path)
    (liii pi compass types)
    (liii pi compass packages)
  ) ;import
  (export PI_COMPASS_COMMON_ENTRY_POINTS PI_COMPASS_ROUTE_DIRS
    PI_COMPASS_CONFIG_FILES PI_COMPASS_SCHEME_LIBRARY_DIRS
    pi-compass-detect-entry-points
  ) ;export
  (begin

    (define PI_COMPASS_COMMON_ENTRY_POINTS
      '(("src/index.ts" "index")
        ("src/index.tsx" "index")
        ("src/index.js" "index")
        ("src/main.ts" "main")
        ("src/main.tsx" "main")
        ("src/main.js" "main")
        ("src/app.ts" "main")
        ("src/app.tsx" "main")
        ("src/app.js" "main")
        ("index.ts" "index")
        ("index.js" "index")
        ("main.go" "main")
        ("cmd/main.go" "main")
        ("src/main.rs" "main")
        ("src/lib.rs" "main")
        ("manage.py" "main")
        ("app.py" "main")
        ("main.py" "main"))
    ) ;define

    (define PI_COMPASS_ROUTE_DIRS
      '("src/routes" "src/pages" "src/api" "app/routes" "app/api" "pages"
        "routes" "api")
    ) ;define

    (define PI_COMPASS_CONFIG_FILES
      '("next.config.js" "next.config.ts" "next.config.mjs" "vite.config.ts"
        "vite.config.js" "webpack.config.js" "webpack.config.ts"
        "nuxt.config.ts" "astro.config.mjs" "xmake.lua")
    ) ;define

    (define PI_COMPASS_SCHEME_LIBRARY_DIRS
      '("liii" "scheme" "srfi" "lib" "libs" "src")
    ) ;define

    (define (%exists? rel cwd)
      (path-exists?
        (path (path->string (path-join (path cwd) rel)))
      ) ;path-exists?
    ) ;define

    (define (%sorted-subdirs dir)
      (catch #t
        (lambda () (list-sort string<? (vector->list (path-list (path dir)))))
        (lambda _ '())
      ) ;catch
    ) ;define

    (define (%has-scm-file? dir)
      (let loop
        ((names (%sorted-subdirs dir)))
        (if (null? names)
          #f
          (let ((full (path->string (path-join (path dir) (car names)))))
            (if
              (and (path-file? (path full)) (string=? (path-suffix (path full)) ".scm"))
              #t
              (loop (cdr names))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%child-lib-entry pdir pname child)
      (let* ((cdir (path->string (path-join (path pdir) child)))
             (rel (string-append pname "/" child))
            ) ;
        (if (and (member child PI_COMPASS_SCHEME_LIBRARY_DIRS)
              (path-dir? (path cdir))
              (%has-scm-file? cdir)
            ) ;and
          (pi-compass-entry-point rel "library")
          #f
        ) ;if
      ) ;let*
    ) ;define

    (define (%append-child-libs pname pdir acc)
      (let loop
        ((children (%sorted-subdirs pdir)) (acc acc))
        (if (null? children)
          acc
          (let ((ep (%child-lib-entry pdir pname (car children))))
            (loop (cdr children) (if ep (cons ep acc) acc))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%parent-lib-entry pname pdir)
      (if (and (member pname PI_COMPASS_SCHEME_LIBRARY_DIRS) (%has-scm-file? pdir))
        (pi-compass-entry-point pname "library")
        #f
      ) ;if
    ) ;define

    (define (%scheme-libraries cwd)
      (let loop
        ((parents (%sorted-subdirs cwd)) (acc '()))
        (if (null? parents)
          (reverse acc)
          (let* ((pname (car parents)) (pdir (path->string (path-join (path cwd) pname))))
            (if (not (path-dir? (path pdir)))
              (loop (cdr parents) acc)
              (let ((ep (%parent-lib-entry pname pdir)))
                (loop (cdr parents) (%append-child-libs pname pdir (if ep (cons ep acc) acc)))
              ) ;let
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%npm-main-entry cwd packages)
      (if
        (not
          (any (lambda (p) (string=? (pi-compass-package-manager p) "npm")) packages)
        ) ;not
        #f
        (let ((pkg
                (pi-compass-read-json (path->string (path-join (path cwd) "package.json")))
              ) ;pkg
             ) ;
          (let ((main (and pkg (pi-compass-jref pkg "main" #f))))
            (if (string? main) main #f)
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    (define (pi-compass-detect-entry-points cwd packages)
      (let* ((main-field (%npm-main-entry cwd packages))
             (results (if main-field (list (pi-compass-entry-point main-field "main")) '()))
             (seen (if main-field (list main-field) '()))
            ) ;
        ;; 常见入口点
        (for-each
          (lambda (pair)
            (let ((path (car pair)) (kind (cadr pair)))
              (when (and (%exists? path cwd) (not (member path seen)))
                (set! results (append results (list (pi-compass-entry-point path kind))))
                (set! seen (cons path seen))
              ) ;when
            ) ;let
          ) ;lambda
          PI_COMPASS_COMMON_ENTRY_POINTS
        ) ;for-each
        ;; 路由目录
        (for-each
          (lambda (dir)
            (when (and (%exists? dir cwd) (not (member dir seen)))
              (set! results (append results (list (pi-compass-entry-point dir "route"))))
              (set! seen (cons dir seen))
            ) ;when
          ) ;lambda
          PI_COMPASS_ROUTE_DIRS
        ) ;for-each
        ;; 配置文件
        (for-each
          (lambda (config)
            (when (and (%exists? config cwd) (not (member config seen)))
              (set! results (append results (list (pi-compass-entry-point config "config"))))
              (set! seen (cons config seen))
            ) ;when
          ) ;lambda
          PI_COMPASS_CONFIG_FILES
        ) ;for-each
        ;; Scheme 适配：库目录 + 库目录规则
        (for-each
          (lambda (ep)
            (let ((p (pi-compass-entry-point-path ep)))
              (when (not (member p seen))
                (set! results (append results (list ep)))
                (set! seen (cons p seen))
              ) ;when
            ) ;let
          ) ;lambda
          (%scheme-libraries cwd)
        ) ;for-each
        results
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
