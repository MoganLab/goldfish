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

    (define (check-unit-test-dirs files)
      "检查所有测试文件所属的目录是否包含 README.md，同级是否存在废弃单测试文件，以及导出函数是否均有测试文件。"
      (let loop
        ((rem files) (seen '()) (viols '()))
        (if (null? rem)
          (reverse viols)
          (let* ((f (car rem))
                 (is-test (string-ends? f "-test.scm"))
                 (parent (if is-test (path->string (path-parent f)) #f))
                ) ;
            (if (or (not is-test) (member parent seen string=?))
              (loop (cdr rem) seen viols)
              (let ((dir-viols (check-single-test-dir parent)))
                (loop (cdr rem) (cons parent seen) (append dir-viols viols))
              ) ;let
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
