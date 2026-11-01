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

(define-library (liii goldcheck unit-test)
  (import (scheme base)
    (scheme file)
    (scheme write)
    (liii base)
    (liii path)
    (liii string)
    (liii list)
    (liii goldcheck path)
  ) ;import
  (export check-unit-test %standard-sections)
  (begin

    (define %standard-sections
      '("语法" "参数" "返回值" "说明" "描述" "注意" "错误处理")
    ) ;define

    (define (divider-string? str)
      (let ((s (string-trim str)))
        (and (> (string-length s) 1)
          (let loop
            ((i 0))
            (if (>= i (string-length s))
              #t
              (if (char=? (string-ref s i) #\-) (loop (+ i 1)) #f)
            ) ;if
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (comment-body line)
      (let ((trimmed (string-trim line)))
        (if (string-starts? trimmed ";")
          (let loop
            ((i 0))
            (if (and (< i (string-length trimmed)) (char=? (string-ref trimmed i) #\;))
              (loop (+ i 1))
              (string-trim (substring trimmed i (string-length trimmed)))
            ) ;if
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

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

    (define (extract-doc-entries lines start-idx)
      (let loop
        ((i start-idx) (acc '()))
        (if (>= i (length lines))
          (reverse acc)
          (let* ((line (list-ref lines i)) (s (string-trim line)))
            (cond
             ((string-starts? s ";;") (loop (+ i 1) (cons (cons (+ i 1) line) acc)))
             ((string-null? s) (loop (+ i 1) acc))
             (else (reverse acc))
            ) ;cond
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (check-doc-elements doc-entries rel-path)
      (let ((fn-name #f)
            (fn-line #f)
            (has-desc #f)
            (has-syntax #f)
            (has-params #f)
            (has-ret #f)
            (first-sec-seen #f)
            (viols '())
            (n (length doc-entries))
           ) ;
        (let loop
          ((k 0))
          (when (< k n)
            (let* ((entry (list-ref doc-entries k))
                   (l-num (car entry))
                   (body (comment-body (cdr entry)))
                   (next-body
                     (if (< (+ k 1) n) (comment-body (cdr (list-ref doc-entries (+ k 1)))) #f)
                   ) ;next-body
                   (has-div (and next-body (divider-string? next-body)))
                  ) ;
              (when (and body (not (string-null? body)))
                (cond
                 ((not fn-name)
                  (if (or (member body %standard-sections) (divider-string? body))
                    (set! viols
                      (cons (list 'unit-test rel-path l-num "缺少函数名声明") viols)
                    ) ;set!
                    (begin
                      (set! fn-name body)
                      (set! fn-line l-num)
                    ) ;begin
                  ) ;if
                 ) ;
                 ((string=? body "示例")
                  (set! first-sec-seen #t)
                  (set! viols
                    (cons (list 'unit-test
                            rel-path
                            l-num
                            "不应包含【示例】段落，因为文档下方的单元测试用例即为可执行示例"
                          ) ;list
                      viols
                    ) ;cons
                  ) ;set!
                 ) ;
                 ((member body %standard-sections)
                  (set! first-sec-seen #t)
                  (unless has-div
                    (set! viols
                      (cons (list 'unit-test
                              rel-path
                              l-num
                              (string-append "小节标题【" body "】下方必须紧跟 '----' 分割线")
                            ) ;list
                        viols
                      ) ;cons
                    ) ;set!
                  ) ;unless
                  (when (string=? body "语法")
                    (set! has-syntax #t)
                  ) ;when
                  (when (string=? body "参数")
                    (set! has-params #t)
                  ) ;when
                  (when (string=? body "返回值")
                    (set! has-ret #t)
                  ) ;when
                 ) ;
                 (has-div (set! first-sec-seen #t)
                   (set! viols
                     (cons (list 'unit-test
                             rel-path
                             l-num
                             (string-append "未知的小节标题【" body "】")
                           ) ;list
                       viols
                     ) ;cons
                   ) ;set!
                 ) ;has-div
                 (else
                   (when (and (not first-sec-seen) (not (divider-string? body)))
                     (set! has-desc #t)
                   ) ;when
                 ) ;else
                ) ;cond
              ) ;when
              (loop (+ k 1))
            ) ;let*
          ) ;when
        ) ;let
        (when (and fn-name (not has-desc))
          (set! viols
            (cons (list 'unit-test rel-path fn-line "缺少函数简要描述") viols)
          ) ;set!
        ) ;when
        (when (not has-syntax)
          (set! viols
            (cons (list 'unit-test rel-path (if fn-line fn-line 1) "缺少【语法】段落")
              viols
            ) ;cons
          ) ;set!
        ) ;when
        (when (not has-params)
          (set! viols
            (cons (list 'unit-test rel-path (if fn-line fn-line 1) "缺少【参数】段落")
              viols
            ) ;cons
          ) ;set!
        ) ;when
        (when (not has-ret)
          (set! viols
            (cons (list 'unit-test rel-path (if fn-line fn-line 1) "缺少【返回值】段落")
              viols
            ) ;cons
          ) ;set!
        ) ;when
        (reverse viols)
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

    (define (check-unit-test file-path)
      "检查单元测试文件结构规范：文档置顶、包含函数名/描述/语法/参数/返回值、末尾为(check-report)且上方有空行。"
      (let* ((rel-path (relativize-path file-path)) (lines (read-all-lines file-path)))
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
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
