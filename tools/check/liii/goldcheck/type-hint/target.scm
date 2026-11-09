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

;; 目标文件（单文件/目录检查）与 hint 库集合的关联分析：
;; - check-dep-hints：import 闭包中各依赖库 hint 的结构校验（第二层）；
;; - reachable-hint-files：检查目标时实际加载类型环境的 hint 集合，
;;   用于状态行按需显示。

(define-library (liii goldcheck type-hint target)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii list)
    (liii string)
    (liii goldcheck path)
    (liii goldcheck type-hint parse)
    (liii goldcheck type-hint util)
    (liii goldcheck type-hint hint-data)
    (liii goldcheck type-hint checks)
    (liii goldcheck type-hint infer)
    (liii goldcheck type-hint env)
    (liii goldcheck type-hint owner)
  ) ;import
  (export check-dep-hints reachable-hint-files)
  (begin

    ;; 源码文件的板块 hint 根：路径含 r7rs/ goldfish/ library/ 段时，
    ;; 取该段同级 hint 目录（与 hint 库的源码根推导互逆）；
    ;; fixture（tests/resources/.../goldfish/liii）同样适用。
    (define (board-hint-root-of-file file)
      (let loop
        ((pre '()) (rest (split-slash (normalize-sep file))))
        (cond ((null? rest) #f)
              ((member (car rest) '("r7rs" "goldfish" "library"))
               (join-strings (append (reverse pre) (list (car rest) "hint")) "/")
              ) ;
              (else (loop (cons (car rest) pre) (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 依赖库的 hint 结构违规 -> 归属被查文件的违规：
    ;; 行号取被查文件的 define-library 行，消息内嵌来源 hint 文件位置
    (define (dep-violations file hf vs)
      (let ((rel-file (relativize-path file)) (rel-hint (relativize-path hf)))
        (map
          (lambda (v)
            (list (car v)
              rel-file
              (list-ref v 2)
              (string-append "["
                rel-hint
                ":"
                (number->string (list-ref v 2))
                "] "
                (list-ref v 3)
              ) ;string-append
            ) ;list
          ) ;lambda
          vs
        ) ;map
      ) ;let
    ) ;define

    ;; 第二层：目标库 import 闭包中各库的 hint 结构校验。
    ;; 与第一层是同一套规则（check-type-hint），只是作用对象不同；
    ;; 依赖 hint 从本地板块根优先查找（fixture 可达），全局根兜底；
    ;; 违规归属被查文件（便于按目标集过滤）。
    (define (check-dep-hints check-hint file)
      (let ((info (cached-library-info file)))
        (if (not info)
          '()
          (let* ((name (lib-name info))
                 (hint? (and (pair? name) (eq? (car name) 'hint)))
                 (local-root (or (and hint? (hint-root-of file)) (board-hint-root-of-file file)))
                 (roots (if local-root (cons local-root (all-hint-roots)) (all-hint-roots)))
                 ;; hint 文件查原库的 import 闭包；源码文件查自身 import 闭包
                 (lib
                   (if hint?
                     (let ((orig (orig-lib-path file)))
                       (and orig (file-exists? orig) (cached-library-info orig))
                     ) ;let
                     info
                   ) ;if
                 ) ;lib
                ) ;
            (if (not lib) '() (check-deps check-hint file (lib-imports lib) roots))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (check-deps check-hint file deps roots)
      (let loop
        ((ds deps) (acc '()))
        (if (null? ds)
          (apply append (reverse acc))
          (let* ((dep (import-library-name (car ds))) (hf (hint-lib-file dep roots)))
            (loop (cdr ds) (if hf (cons (dep-violations file hf (check-hint hf)) acc) acc))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 目标文件可达的 hint 集合（用于状态行按需显示）：
    ;; hint 文件取自身 + 原库 import 闭包的 hint；
    ;; 普通源码文件取自身及 import 闭包的 hint；
    ;; (scheme base) 是 build-env/build-import-env 无条件附加的内建层，
    ;; 其 hint 总是可达。
    (define (closure-hint-files info roots)
      (filter string?
        (map
          (lambda (c)
            (let ((h (hint-lib-file (lib-name (cdr c)) roots)))
              (if h (relativize-path h) #f)
            ) ;let
          ) ;lambda
          (source-closure info roots)
        ) ;map
      ) ;filter
    ) ;define

    (define (builtin-hint-file)
      (let ((b (hint-lib-file '(scheme base) (all-hint-roots))))
        (if b (relativize-path b) #f)
      ) ;let
    ) ;define

    (define (reachable-hint-of file acc)
      (let ((info (cached-library-info file)))
        (if (not info)
          acc
          (let* ((name (lib-name info))
                 (hint? (and (pair? name) (eq? (car name) 'hint)))
                 (roots (all-hint-roots))
                 (orig (and hint? (orig-lib-path file)))
                 (oinfo (and orig (file-exists? orig) (cached-library-info orig)))
                 (own
                   (if hint?
                     (cons (relativize-path file) (if oinfo (closure-hint-files oinfo roots) '()))
                     (closure-hint-files info roots)
                   ) ;if
                 ) ;own
                 (with-builtin
                   (let ((b (builtin-hint-file)))
                     (if b (cons b own) own)
                   ) ;let
                 ) ;with-builtin
                ) ;
            (append with-builtin acc)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (reachable-hint-files files)
      "返回检查目标文件时实际加载类型环境的 hint 库文件列表（去重）。\n      与 display-type-hint-statuses 配合，只显示与目标相关的状态行。"
      (let loop
        ((fs files) (acc '()))
        (if (null? fs)
          (dedup-paths (reverse acc))
          (loop (cdr fs) (reachable-hint-of (car fs) acc))
        ) ;if
      ) ;let
    ) ;define

    (define (dedup-paths rest)
      (let loop
        ((xs rest) (out '()))
        (if (null? xs)
          (reverse out)
          (loop (cdr xs) (if (member (car xs) out string=?) out (cons (car xs) out)))
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
