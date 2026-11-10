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

(define-library (liii goldcheck unit-test dirs)
  (import (scheme base)
    (liii base)
    (liii path)
    (liii string)
    (liii list)
    (liii hash-table)
    (liii comparator)
    (liii goldcheck path)
    (liii goldcheck unit-test file)
    (liii goldcheck unit-test source)
  ) ;import
  (export check-unit-test-dirs)
  (begin

    (define (check-dir-legacy-file parent rel-parent readme-rel)
      (let* ((norm-parent (normalize-sep parent))
             (legacy-path-str (string-append norm-parent "-test.scm"))
             (legacy-rel (string-append rel-parent "-test.scm"))
            ) ;
        (if (path-file? (path legacy-path-str))
          (let ((lib-name (parent->lib-name rel-parent)))
            (list (list 'unit-test
                    legacy-rel
                    1
                    (string-append lib-name " 的文档，现在是 " readme-rel
                      " ，" legacy-rel " 这个方案已经废弃"
                    ) ;string-append
                  ) ;list
            ) ;list
          ) ;let
          '()
        ) ;if
      ) ;let*
    ) ;define

    (define (check-dir-readme readme)
      (if (path-file? readme)
        '()
        (list (list 'unit-test
                (relativize-path (path->string readme))
                1
                "缺少库导览文档 README.md"
              ) ;list
        ) ;list
      ) ;if
    ) ;define

    (define (check-single-test-dir parent)
      ;; 目录不存在（仅由源码反向推导出的期望路径）时，文档性检查
      ;; （README、废弃单文件）无意义；但缺失单测的报告必须保留，
      ;; 否则无测试目录的库会被漏报。
      (let* ((rel-parent (relativize-path parent))
             (doc-viols
               (if (path-dir? (path parent))
                 (let ((readme (path-join parent "README.md"))
                       (readme-rel (string-append rel-parent "/README.md"))
                      ) ;
                   (append (check-dir-legacy-file parent rel-parent readme-rel)
                     (check-dir-readme readme)
                   ) ;append
                 ) ;let
                 '()
               ) ;if
             ) ;doc-viols
             (v3 (check-missing-exported-tests parent rel-parent))
            ) ;
        (append doc-viols v3)
      ) ;let*
    ) ;define

    (define (resolve-file-test-dir f)
      (cond
       ((unit-test-file? f) (path->string (path-parent (path f))))
       (else (find-test-dir-for-library-source f))
      ) ;cond
    ) ;define

    ;; 把文件集合一次性规范化为已排序向量，成员查询用二分，
    ;; 避免 check-dir-test-files 对每个条目线性扫描整个文件列表。
    (define (files->norm-set files)
      (list->vector
        (sort!
          (map (lambda (f) (normalize-sep (relativize-path f))) files)
          string<?
        ) ;sort!
      ) ;list->vector
    ) ;define

    (define (norm-set-contains? norm-set target)
      (let ((lo 0) (hi (vector-length norm-set)) (found #f))
        (let loop
          ()
          (when (< lo hi)
            (let* ((mid (quotient (+ lo hi) 2)) (v (vector-ref norm-set mid)))
              (cond ((string=? v target) (set! found #t))
                    ((string<? v target) (set! lo (+ mid 1)) (loop))
                    (else (set! hi mid) (loop))
              ) ;cond
            ) ;let*
          ) ;when
        ) ;let
        found
      ) ;let
    ) ;define

    (define (check-dir-test-files parent norm-set)
      (if (not (path-dir? (path parent)))
        '()
        (let ((entries (path-list (path parent))))
          (let loop
            ((i 0) (acc '()))
            (if (>= i (vector-length entries))
              acc
              (let* ((entry (vector-ref entries i)) (full (path->string (path-join parent entry))))
                (if
                  (and (string-ends? entry "-test.scm")
                    (not (norm-set-contains? norm-set (normalize-sep (relativize-path full))))
                  ) ;and
                  (loop (+ i 1) (append (check-unit-test full) acc))
                  (loop (+ i 1) acc)
                ) ;if
              ) ;let*
            ) ;if
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    (define (check-unit-test-dirs files)
      "检查所有测试文件或源码文件所属的测试目录：包含 README.md，无废弃单测试文件，导出函数均有单测，且目录下单测文件合规。"
      (let ((norm-set (files->norm-set files)))
        (let loop
          ((rem files) (seen '()) (viols '()))
          (if (null? rem)
            (reverse viols)
            (let* ((f (car rem))
                   (parent (resolve-file-test-dir f))
                   (norm-parent (if parent (normalize-sep (relativize-path parent)) #f))
                  ) ;
              (if (or (not parent) (member norm-parent seen string=?))
                (loop (cdr rem) seen viols)
                (let* ((dir-viols (check-single-test-dir parent))
                       (file-viols (check-dir-test-files parent norm-set))
                       (all-dir-viols (append file-viols dir-viols))
                      ) ;
                  (loop (cdr rem) (cons norm-parent seen) (append all-dir-viols viols))
                ) ;let*
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
