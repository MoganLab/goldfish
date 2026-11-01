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

(define-library (liii goldcheck unit-test doc)
  (import (scheme base) (liii base) (liii string) (liii list))
  (export %standard-sections divider-string? comment-body extract-doc-entries
    check-doc-elements
  ) ;export
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

    (define (check-doc-body body next-body l-num rel-path state)
      ;; state: (fn-name fn-line has-desc has-syntax has-params has-ret first-sec-seen viols)
      (let ((fn-name (list-ref state 0))
            (fn-line (list-ref state 1))
            (has-desc (list-ref state 2))
            (has-syntax (list-ref state 3))
            (has-params (list-ref state 4))
            (has-ret (list-ref state 5))
            (first-sec (list-ref state 6))
            (viols (list-ref state 7))
            (has-div (and next-body (divider-string? next-body)))
           ) ;
        (cond
         ((not fn-name)
          (if (or (member body %standard-sections) (divider-string? body))
            (list #f
              fn-line
              has-desc
              has-syntax
              has-params
              has-ret
              first-sec
              (cons (list 'unit-test rel-path l-num "缺少函数名声明") viols)
            ) ;list
            (list body l-num has-desc has-syntax has-params has-ret first-sec viols)
          ) ;if
         ) ;
         ((string=? body "示例")
          (list fn-name
            fn-line
            has-desc
            has-syntax
            has-params
            has-ret
            #t
            (cons (list 'unit-test
                    rel-path
                    l-num
                    "不应包含【示例】段落，因为文档下方的单元测试用例即为可执行示例"
                  ) ;list
              viols
            ) ;cons
          ) ;list
         ) ;
         ((member body %standard-sections)
          (let ((v
                  (if has-div
                    viols
                    (cons (list 'unit-test
                            rel-path
                            l-num
                            (string-append "小节标题【" body "】下方必须紧跟 '----' 分割线")
                          ) ;list
                      viols
                    ) ;cons
                  ) ;if
                ) ;v
               ) ;
            (list fn-name
              fn-line
              has-desc
              (or has-syntax (string=? body "语法"))
              (or has-params (string=? body "参数"))
              (or has-ret (string=? body "返回值"))
              #t
              v
            ) ;list
          ) ;let
         ) ;
         (has-div
           (list fn-name
             fn-line
             has-desc
             has-syntax
             has-params
             has-ret
             #t
             (cons (list 'unit-test
                     rel-path
                     l-num
                     (string-append "未知的小节标题【" body "】")
                   ) ;list
               viols
             ) ;cons
           ) ;list
         ) ;has-div
         (else
           (let ((desc? (or has-desc (and (not first-sec) (not (divider-string? body))))))
             (list fn-name fn-line desc? has-syntax has-params has-ret first-sec viols)
           ) ;let
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (validate-doc-required-sections state rel-path)
      (let ((fn-name (list-ref state 0))
            (fn-line (list-ref state 1))
            (has-desc (list-ref state 2))
            (has-syntax (list-ref state 3))
            (has-params (list-ref state 4))
            (has-ret (list-ref state 5))
            (viols (list-ref state 7))
            (fallback-line (if (list-ref state 1) (list-ref state 1) 1))
           ) ;
        (let* ((v1 (if (and fn-name (not has-desc))
                     (cons (list 'unit-test rel-path fn-line "缺少函数简要描述") viols)
                     viols
                   ) ;if
               ) ;v1
               (v2 (if (not has-syntax)
                     (cons (list 'unit-test rel-path fallback-line "缺少【语法】段落") v1)
                     v1
                   ) ;if
               ) ;v2
               (v3 (if (not has-params)
                     (cons (list 'unit-test rel-path fallback-line "缺少【参数】段落") v2)
                     v2
                   ) ;if
               ) ;v3
               (v4 (if (not has-ret)
                     (cons (list 'unit-test rel-path fallback-line "缺少【返回值】段落") v3)
                     v3
                   ) ;if
               ) ;v4
              ) ;
          (reverse v4)
        ) ;let*
      ) ;let
    ) ;define

    (define (check-doc-elements doc-entries rel-path)
      (let ((n (length doc-entries)))
        (let loop
          ((k 0) (state (list #f #f #f #f #f #f #f '())))
          (if (>= k n)
            (validate-doc-required-sections state rel-path)
            (let* ((entry (list-ref doc-entries k))
                   (l-num (car entry))
                   (body (comment-body (cdr entry)))
                   (next-body
                     (if (< (+ k 1) n) (comment-body (cdr (list-ref doc-entries (+ k 1)))) #f)
                   ) ;next-body
                  ) ;
              (if (and body (not (string-null? body)))
                (loop (+ k 1) (check-doc-body body next-body l-num rel-path state))
                (loop (+ k 1) state)
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
