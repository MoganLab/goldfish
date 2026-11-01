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

(define-library (liii goldcheck unit-test file)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii string)
    (liii path)
    (liii goldcheck path)
    (liii goldcheck unit-test doc)
  ) ;import
  (export read-all-lines find-first-non-empty-index find-last-non-empty-index
    check-report-tail parent->lib-name check-unit-test-file-content
    check-unit-test
  ) ;export
  (begin

    (define (read-all-lines file-path)
      (let ((port (open-input-file file-path)))
        (dynamic-wind (lambda () #f)
          (lambda ()
            (let loop
              ((acc '()))
              (let ((line (read-line port)))
                (if (eof-object? line) (reverse acc) (loop (cons line acc)))
              ) ;let
            ) ;let
          ) ;lambda
          (lambda () (close-port port))
        ) ;dynamic-wind
      ) ;let
    ) ;define

    (define (find-first-non-empty-index lines)
      (let loop
        ((i 0))
        (cond ((>= i (length lines)) #f)
              ((not (string-null? (string-trim (list-ref lines i)))) i)
              (else (loop (+ i 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (find-last-non-empty-index lines)
      (let loop
        ((i (- (length lines) 1)))
        (cond ((< i 0) #f)
              ((not (string-null? (string-trim (list-ref lines i)))) i)
              (else (loop (- i 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (check-report-tail lines rel-path)
      (let ((last-idx (find-last-non-empty-index lines)))
        (cond
         ((not last-idx)
          (list (list 'unit-test
                  rel-path
                  (length lines)
                  "(check-report) 必须位于文件最后一行"
                ) ;list
          ) ;list
         ) ;
         ((not (string=? (string-trim (list-ref lines last-idx)) "(check-report)"))
          (list (list 'unit-test
                  rel-path
                  (+ last-idx 1)
                  "(check-report) 必须位于文件最后一行"
                ) ;list
          ) ;list
         ) ;
         ((or (<= last-idx 0)
            (not
              (string-null? (string-trim (list-ref lines (- last-idx 1))))
            ) ;not
          ) ;or
          (list (list 'unit-test
                  rel-path
                  (+ last-idx 1)
                  "(check-report) 上方必须保留一行空行"
                ) ;list
          ) ;list
         ) ;
         (else '())
        ) ;cond
      ) ;let
    ) ;define

    (define (parent->lib-name rel-parent)
      (let* ((norm (normalize-sep rel-parent))
             (clean (if (string-starts-with? norm "tests/")
                      (substring norm 6 (string-length norm))
                      norm
                    ) ;if
             ) ;clean
            ) ;
        (string-append "(" (string-join (string-split clean "/") " ") ")")
      ) ;let*
    ) ;define

    (define (check-unit-test-file-content file-path rel-path)
      (let ((lines (read-all-lines file-path)))
        (if (null? lines)
          (list (list 'unit-test rel-path 1 "空测试文件"))
          (let ((first-idx (find-first-non-empty-index lines)))
            (if
              (or (not first-idx)
                (not (string-starts? (string-trim (list-ref lines first-idx)) ";;"))
              ) ;or
              (list (list 'unit-test
                      rel-path
                      (if first-idx (+ first-idx 1) 1)
                      "文档必须置于文件最顶部（在 import 之前）"
                    ) ;list
              ) ;list
              (append (check-doc-elements (extract-doc-entries lines first-idx) rel-path)
                (check-report-tail lines rel-path)
              ) ;append
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (check-unit-test file-path)
      "检查单元测试文件结构规范：文档置顶、包含函数名/描述/语法/参数/返回值、末尾为(check-report)且上方有空行。"
      (let* ((rel-path (relativize-path file-path)) (norm-path (normalize-sep file-path)))
        (if (string-ends? norm-path "-test.scm")
          (let* ((dir-path (substring norm-path 0 (- (string-length norm-path) 9)))
                 (dir-rel (substring rel-path 0 (- (string-length rel-path) 9)))
                ) ;
            (if (path-dir? (path dir-path))
              (let* ((lib-name (parent->lib-name dir-rel))
                     (readme-rel (string-append dir-rel "/README.md"))
                    ) ;
                (list (list 'unit-test
                        rel-path
                        1
                        (string-append lib-name " 的文档，现在是 "
                          readme-rel " ，" rel-path " 这个方案已经废弃"
                        ) ;string-append
                      ) ;list
                ) ;list
              ) ;let*
              (check-unit-test-file-content file-path rel-path)
            ) ;if
          ) ;let*
          (check-unit-test-file-content file-path rel-path)
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
