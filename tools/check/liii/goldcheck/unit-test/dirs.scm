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
      (let* ((rel-parent (relativize-path parent))
             (readme (path-join parent "README.md"))
             (readme-rel (string-append rel-parent "/README.md"))
             (v1 (check-dir-legacy-file parent rel-parent readme-rel))
             (v2 (check-dir-readme readme))
             (v3 (check-missing-exported-tests parent rel-parent))
            ) ;
        (append v1 v2 v3)
      ) ;let*
    ) ;define

    (define (resolve-file-test-dir f)
      (cond
       ((unit-test-file? f) (path->string (path-parent (path f))))
       (else (find-test-dir-for-library-source f))
      ) ;cond
    ) ;define

    ;; 把文件集合一次性规范化为字符串哈希集合，成员查询 O(1)，
    ;; 避免 check-dir-test-files 对每个条目线性扫描整个文件列表。
    (define (files->norm-set files)
      (let ((ht (make-hash-table (make-comparator string? string=? #f string-hash))))
        (for-each
          (lambda (f) (hash-table-set! ht (normalize-sep (relativize-path f)) #t))
          files
        ) ;for-each
        ht
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
                    (not (hash-table-contains? norm-set (normalize-sep (relativize-path full))))
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
