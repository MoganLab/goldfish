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
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; 依据当前 *load-path* 推导每个库加载根目录对应的 tests 根目录，
;; 并把扫描得到的库导出索引写入 <tests 根目录>/function-library-index.json。

(define-library (liii golddoc index-build)
  (import (scheme base)
    (liii golddoc index-source)
    (liii golddoc library)
    (liii json)
    (liii path)
  ) ;import
  (export build-function-indexes!)
  (begin

    ;; 当前工作目录只是回退用的加载根目录：在子目录里执行 --build-json 时
    ;; 不应把 cwd 当成库根目录，否则会在无关目录（例如 tools/tests/）里
    ;; 生成多余的索引文件。
    (define (cwd-load-root? load-root)
      (or (string=? load-root ".") (string=? load-root "./") (string=? load-root ""))
    ) ;define

    ;; 索引目标：加载根目录 → 关联的 tests 根目录。
    ;; 多个加载根目录可能指向同一个 tests 根目录（例如仓库根目录与
    ;; goldfish/ 目录都回退到仓库级 tests/），必须合并后只写一次，
    ;; 否则后写入的一方会覆盖另一方的导出记录。
    (define (collect-index-target-pairs)
      (let loop
        ((roots *load-path*) (pairs '()))
        (if (null? roots)
          pairs
          (let* ((load-root (car roots))
                 (tests-root (and (string? load-root) (find-tests-root-for-load-root load-root)))
                ) ;
            (loop (cdr roots)
              (if (and tests-root (not (cwd-load-root? load-root)))
                (append pairs (list (cons tests-root load-root)))
                pairs
              ) ;if
            ) ;loop
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (index-target-key tests-root)
      (path->string (path-resolve tests-root))
    ) ;define

    (define (same-index-target-key? key pair)
      (string=? (index-target-key (car pair)) key)
    ) ;define

    (define (group-index-targets pairs)
      (let loop
        ((remaining pairs) (result '()))
        (if (null? remaining)
          result
          (let* ((tests-root (caar remaining))
                 (key (index-target-key tests-root))
                 (load-roots '())
                 (others '())
                ) ;
            (let collect
              ((items remaining))
              (if (null? items)
                #f
                (if (same-index-target-key? key (car items))
                  (begin
                    (set! load-roots (append load-roots (list (cdr (car items)))))
                    (collect (cdr items))
                  ) ;begin
                  (begin
                    (set! others (append others (list (car items))))
                    (collect (cdr items))
                  ) ;begin
                ) ;if
              ) ;if
            ) ;let
            (loop others (append result (list (cons tests-root load-roots))))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (build-function-index-at! tests-root load-roots)
      (let ((index-path (path->string (path-join tests-root "function-library-index.json")))
           ) ;
        (let* ((raw-index (build-index-for-load-roots load-roots))
               (json-value (index->json-value raw-index))
               ;; 当索引为空时，使用空对象 '(()) 代替空列表
               (normalized-json-value (if (null? json-value) '(()) json-value))
              ) ;
          (path-write-text index-path (json->string normalized-json-value))
        ) ;let*
        index-path
      ) ;let
    ) ;define

    (define (build-function-indexes!)
      (let loop
        ((targets (group-index-targets (collect-index-target-pairs))) (built-paths '()))
        (if (null? targets)
          built-paths
          (let* ((target (car targets)) (tests-root (car target)) (load-roots (cdr target)))
            (loop (cdr targets)
              (append built-paths (list (build-function-index-at! tests-root load-roots)))
            ) ;loop
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
