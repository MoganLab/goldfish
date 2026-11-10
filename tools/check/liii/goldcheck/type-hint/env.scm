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

;; 类型环境加载：沿 import/再导出关系穿透查找过程签名。

(define-library (liii goldcheck type-hint env)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii list)
    (liii string)
    (liii gfproject config)
    (liii goldcheck path)
    (liii goldcheck type-hint parse)
    (liii goldcheck type-hint util)
    (liii goldcheck type-hint hint-data)
  ) ;import
  (export build-env build-import-env all-hint-roots full-coverage? hint-roots-of
    hint-root-of library-file source-closure defining-libs exporting-libs
    hint-lib-file cached-library-info import-library-name
  ) ;export
  (begin

    (define (hint-root-of hint-file)
      (let loop
        ((pre '()) (rest (split-slash (normalize-sep hint-file))))
        (cond ((null? rest) #f)
              ((string=? (car rest) "hint")
               (join-strings (append (reverse pre) '("hint")) "/")
              ) ;
              (else (loop (cons (car rest) pre) (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 全局 hint 根：按 domain 推导（与 tests 目录同源），
    ;; 各板块的 hint 库分别位于 r7rs/hint、goldfish/hint、library/hint
    ;; 与 tools/<tool>/hint 下，推断时需要跨板块查找对应库的 hint 与原库。
    (define (global-hint-roots)
      (gfproject-hint-dirs)
    ) ;define

    (define (board-hint-roots prefix)
      (map
        (lambda (g) (join-strings (append prefix (split-slash g)) "/"))
        (global-hint-roots)
      ) ;map
    ) ;define

    ;; hint 文件关联的全局 hint 根：
    ;; 工具 hint 根（tools/<name>/hint）取 tools 段之前的路径为前缀，
    ;; 板块 hint 根（<板块>/hint）取板块目录的同级为前缀。
    (define (global-hint-roots-of hint-file)
      (let loop
        ((pre '()) (rest (split-slash (normalize-sep hint-file))))
        (cond ((null? rest) '())
              ((and (string=? (car rest) "tools")
                 (pair? (cdr rest))
                 (pair? (cddr rest))
                 (string=? (caddr rest) "hint")
               ) ;and
               (board-hint-roots (reverse pre))
              ) ;
              ((string=? (car rest) "hint") (board-hint-roots (reverse (cdr (reverse pre)))))
              (else (loop (cons (car rest) pre) (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (dedup-strings xs)
      (let loop
        ((rest xs) (acc '()))
        (if (null? rest)
          (reverse acc)
          (if (member (car rest) acc string=?)
            (loop (cdr rest) acc)
            (loop (cdr rest) (cons (car rest) acc))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (hint-roots-of hint-file)
      (let ((local (hint-root-of hint-file)))
        (if (not local)
          '()
          (dedup-strings (cons local (global-hint-roots-of hint-file)))
        ) ;if
      ) ;let
    ) ;define

    ;; ---------- 归属库分析（A7） ----------

    ;; 路径 -> 库信息 的缓存，避免同一文件被重复解析
    (define %lib-info-cache '())

    (define (cached-library-info path)
      (if (not (and path (file-exists? path)))
        #f
        (let ((hit (assq path %lib-info-cache)))
          (if hit
            (cdr hit)
            (let ((info (parse-library-file path)))
              (set! %lib-info-cache (cons (cons path info) %lib-info-cache))
              info
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; 原库及其 import 闭包，按广度优先顺序返回 (深度 . 库信息)；
    ;; 深度 0 为原库自身。
    (define (source-closure oinfo roots)
      (let loop
        ((queue (lib-imports oinfo))
         (depth 1)
         (acc (list (cons 0 oinfo)))
         (seen (list (lib-name oinfo)))
        ) ;
        (if (null? queue)
          (reverse acc)
          (let ((lib (car queue)))
            (if (member lib seen equal?)
              (loop (cdr queue) depth acc seen)
              (let ((info (cached-library-info (library-file roots lib #t))))
                (if (not info)
                  (loop (cdr queue) depth acc (cons lib seen))
                  (loop (append (cdr queue) (lib-imports info))
                    (+ depth 1)
                    (cons (cons depth info) acc)
                    (cons lib seen)
                  ) ;loop
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 闭包中本地 define 了 name 的库，按广度优先顺序
    (define (defining-libs name closure)
      (filter
        (lambda (c) (assq name (lib-defines (cdr c))))
        closure
      ) ;filter
    ) ;define

    ;; 闭包中导出了 name 的库，按广度优先顺序
    (define (exporting-libs name closure)
      (filter
        (lambda (c) (memq name (lib-exports (cdr c))))
        closure
      ) ;filter
    ) ;define

    ;; 库对应的 hint 库文件路径；不存在返回 #f
    (define (hint-lib-file lib-name roots)
      (library-file roots lib-name #f)
    ) ;define

    (define (entry->binding e vars)
      (let ((d (find-def (cadr e) vars)))
        (cons (car e) (if d (signature-value d) #f))
      ) ;let
    ) ;define

    (define (hint-env-entries info)
      (let* ((qname (query-func-name (lib-name info)))
             (vars (hint-var-defs info))
             (entries (registered-entries info qname))
            ) ;
        (filter
          (lambda (kv) (and (cdr kv) (list? (cdr kv))))
          (map (lambda (e) (entry->binding e vars)) entries)
        ) ;filter
      ) ;let*
    ) ;define

    (define (import-library-name clause)
      (if (and (pair? clause)
            (memq (car clause) '(except only prefix rename for library))
          ) ;and
        (import-library-name (cadr clause))
        clause
      ) ;if
    ) ;define

    (define (source-roots-of hint-root)
      ;; hint 根 -> 原库源码根列表。
      ;; 板块 hint 根（r7rs/hint、goldfish/hint）下登记的 hint 可能对应任一
      ;; 板块的库，因此依次尝试 r7rs/ goldfish/ library/；工具 hint 根
      ;; （tools/<tool>/hint）的原库位于同层工具目录。
      (let* ((comps (split-slash (normalize-sep hint-root)))
             (source
               (if
                 (and (pair? comps) (string=? (car (reverse comps)) "hint"))
                 (reverse (cdr (reverse comps)))
                 #f
               ) ;if
             ) ;source
            ) ;
        (cond ((not source) (list hint-root))
              ((null? source) (list "."))
              ((and (= (length source) 1) (member (car source) '("r7rs"
                                                                 "goldfish")))
               '("r7rs" "goldfish" "library")
              ) ;
              (else (list (join-strings source "/")))
        ) ;cond
      ) ;let*
    ) ;define

    (define (library-file-in-root root lib-syms)
      (let ((path (string-append root "/" (join-strings (map symbol->string lib-syms) "/") ".scm")
            ) ;path
           ) ;
        (if (file-exists? path) path #f)
      ) ;let
    ) ;define

    (define (library-file roots lib-syms source?)
      (if (null? roots)
        #f
        (let ((cands (if source? (source-roots-of (car roots)) (list (car roots)))))
          (let loop
            ((cs cands))
            (if (null? cs)
              (library-file (cdr roots) lib-syms source?)
              (let ((hit (library-file-in-root (car cs) lib-syms)))
                (if hit hit (loop (cdr cs)))
              ) ;let
            ) ;if
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    (define (parse-library-at path)
      (and path (cached-library-info path))
    ) ;define

    (define (public-bindings bindings oinfo)
      (if oinfo
        (filter (lambda (binding) (memq (car binding) (lib-exports oinfo))) bindings)
        bindings
      ) ;if
    ) ;define

    (define (dependency-env imports roots visited)
      (let loop
        ((libs imports) (env '()) (seen visited))
        (if (null? libs)
          (list env seen)
          (let ((result (load-library-env (import-library-name (car libs)) roots seen)))
            (loop (cdr libs) (append env (car result)) (cadr result))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 库级类型环境缓存：lib -> 环境绑定列表。
    ;; 批量检查源码文件时同一库的环境会被反复请求，避免重复解析。
    (define %lib-env-cache '())

    ;; 返回 (类型环境 已访问库)：沿原库 import 递归，保留公开导出绑定。
    ;; 已访问（环或共享依赖）时返回空环境，保持首次计算的顺序语义。
    (define (load-library-env lib-syms roots visited)
      (if (member lib-syms visited equal?)
        (list '() visited)
        (let ((hit (assoc lib-syms %lib-env-cache equal?)))
          (if hit
            (list (cdr hit) (cons lib-syms visited))
            (let* ((seen (cons lib-syms visited))
                   (hint-info (parse-library-at (library-file roots lib-syms #f)))
                   (oinfo (parse-library-at (library-file roots lib-syms #t)))
                   (own (if hint-info (hint-env-entries hint-info) '()))
                   (deps (dependency-env (if oinfo (lib-imports oinfo) '()) roots seen))
                   (env (public-bindings (append own (car deps)) oinfo))
                  ) ;
              (set! %lib-env-cache (cons (cons lib-syms env) %lib-env-cache))
              (list env (cadr deps))
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; 类型环境：自身 hint 登记 + import 穿透 + s7 内建基础过程。
    ;; 自身登记优先，其次 import，最后内建层（car/cdr/string-append 等
    ;; 在 s7 中全局可见，不依赖 import）。
    (define (build-env oinfo hint-file)
      (let* ((roots (hint-roots-of hint-file))
             (own-info (parse-library-file hint-file))
             (own (if own-info (hint-env-entries own-info) '()))
             (deps (car (dependency-env (lib-imports oinfo) roots '())))
             (builtins (car (load-library-env '(scheme base) roots '())))
            ) ;
        (append own deps builtins)
      ) ;let*
    ) ;define

    ;; 导出全覆盖：原库每个导出名字都在类型环境中有签名。
    (define (full-coverage? env oinfo)
      (every (lambda (name) (assq name env)) (lib-exports oinfo))
    ) ;define

    ;; 仓库根前缀：优先 g_project-root；测试子进程等场景下不可用时，
    ;; 从 cwd 向上探测仓库根（含 goldfish/ 与 gf_check.json 的目录）
    (define (repo-root-prefix)
      (let ((root (or (gfproject-root) (find-repo-root))))
        (if (and root (string? root) (not (string=? root "")))
          (if (string-ends? root "/") root (string-append root "/"))
          ""
        ) ;if
      ) ;let
    ) ;define

    ;; 仓库全部 hint 根（带仓库前缀）：检查任意源码文件时按 import 定位 hint。
    (define (all-hint-roots)
      (let ((prefix (repo-root-prefix)))
        (map (lambda (d) (string-append prefix d)) (global-hint-roots))
      ) ;let
    ) ;define

    ;; 源码文件 import 列表 -> 类型环境：依赖穿透 + scheme base 内建层。
    ;; 与 build-env 的差别：不含自身 hint 登记（任意源码文件通常没有 hint）。
    (define (build-import-env imports)
      (let* ((roots (all-hint-roots))
             (deps (car (dependency-env imports roots '())))
             (builtins (car (load-library-env '(scheme base) roots '())))
            ) ;
        (append deps builtins)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
