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
    (liii goldcheck path)
    (liii goldcheck type-hint parse)
    (liii goldcheck type-hint util)
    (liii goldcheck type-hint hint-data)
  ) ;import
  (export build-env full-coverage?)
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

    ;; tools/<name>/hint 与同级的 goldfish/hint 对应。
    (define (tool-goldfish-hint-root hint-file)
      (let loop
        ((pre '()) (rest (split-slash (normalize-sep hint-file))))
        (cond ((null? rest) #f)
              ((and (string=? (car rest) "tools")
                 (pair? (cdr rest))
                 (pair? (cddr rest))
                 (string=? (caddr rest) "hint")
               ) ;and
               (join-strings (append (reverse pre) '("goldfish" "hint")) "/")
              ) ;
              (else (loop (cons (car rest) pre) (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (hint-roots-of hint-file)
      (let ((local (hint-root-of hint-file)) (global (tool-goldfish-hint-root hint-file)))
        (append (if local (list local) '()) (if global (list global) '()))
      ) ;let
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

    (define (source-root-of hint-root)
      (let ((comps (split-slash (normalize-sep hint-root))))
        (if
          (and (pair? comps) (string=? (car (reverse comps)) "hint"))
          (let ((source (reverse (cdr (reverse comps)))))
            (if (null? source) "." (join-strings source "/"))
          ) ;let
          hint-root
        ) ;if
      ) ;let
    ) ;define

    (define (import-library-name clause)
      (if (and (pair? clause)
            (memq (car clause) '(except only prefix rename for library))
          ) ;and
        (import-library-name (cadr clause))
        clause
      ) ;if
    ) ;define

    (define (library-file roots lib-syms source?)
      (if (null? roots)
        #f
        (let* ((root (if source? (source-root-of (car roots)) (car roots)))
               (path (string-append root "/" (join-strings (map symbol->string lib-syms) "/") ".scm")
               ) ;path
              ) ;
          (if (file-exists? path) path (library-file (cdr roots) lib-syms source?))
        ) ;let*
      ) ;if
    ) ;define

    (define (parse-library-at path)
      (and path (parse-library-file path))
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

    ;; 返回 (类型环境 已访问库)：沿原库 import 递归，保留公开导出绑定。
    (define (load-library-env lib-syms roots visited)
      (if (member lib-syms visited equal?)
        (list '() visited)
        (let* ((seen (cons lib-syms visited))
               (hint-info (parse-library-at (library-file roots lib-syms #f)))
               (oinfo (parse-library-at (library-file roots lib-syms #t)))
               (own (if hint-info (hint-env-entries hint-info) '()))
               (deps (dependency-env (if oinfo (lib-imports oinfo) '()) roots seen))
              ) ;
          (list (public-bindings (append own (car deps)) oinfo) (cadr deps))
        ) ;let*
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

  ) ;begin
) ;define-library
