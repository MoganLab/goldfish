
;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(define-library (liii goldeval)
  (import (scheme base)
          (scheme process-context)
          (liii list)
          (liii string)
          (liii argparse)
  ) ;import
  (export parse-eval-args
          main
  ) ;export
  (begin

    (define (count-expressions str)
      ;; 计算字符串中包含的顶层S表达式数量
      ;; 通过跟踪括号深度来计数
      (let loop ((chars (string->list str))
                 (depth 0)
                 (in-expr #f)
                 (count 0))
        (if (null? chars)
          (if in-expr (+ count 1) count)
          (let ((c (car chars)))
            (cond
              ((char=? c #\()
               (loop (cdr chars) (+ depth 1) #t count)
              ) ;
              ((char=? c #\))
               (let ((new-depth (- depth 1)))
                 (if (and (= new-depth 0) in-expr)
                   (loop (cdr chars) new-depth #f (+ count 1))
                   (loop (cdr chars) new-depth in-expr count)
                 ) ;if
               ) ;let
              ) ;
              (else
                (loop (cdr chars) depth in-expr count)
              ) ;else
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (extract-code-args args)
      ;; 从参数列表中提取代码参数（非选项参数）
      ;; 跳过 -m/--mode 及其值，以及 "eval" 命令
      (let loop ((remaining args)
                 (skip-next #f)
                 (code-args '()))
        (if (null? remaining)
          (reverse code-args)
          (let ((arg (car remaining)))
            (cond
              (skip-next
               (loop (cdr remaining) #f code-args)
              ) ;skip-next
              ((equal? arg "eval")
               (loop (cdr remaining) #f code-args)
              ) ;
              ((or (equal? arg "-m") (equal? arg "--mode"))
               (loop (cdr remaining) #t code-args)
              ) ;
              ;; argparse 不支持 -m=value 格式，这里需要处理
              ((string-starts? arg "-m=")
               (loop (cdr remaining) #f code-args)
              ) ;
              ((string-starts? arg "--mode=")
               (loop (cdr remaining) #f code-args)
              ) ;
              ((string-starts? arg "-")
               (loop (cdr remaining) #f code-args)
              ) ;
              (else
                (loop (cdr remaining) #f (cons arg code-args))
              ) ;else
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (combine-code code-args)
      ;; 将多个代码参数组合成单个表达式
      (cond
        ((null? code-args) #f)
        ((= (length code-args) 1)
         (let ((code (car code-args)))
           (if (> (count-expressions code) 1)
             (string-append "(begin " code ")")
             code
           ) ;if
         ) ;let
        ) ;
        (else
          ;; 多个表达式，用 (begin ...) 包裹
          (string-append "(begin "
                         (string-join code-args " ")
                         ")"
          ) ;string-append
        ) ;else
      ) ;cond
    ) ;define

    (define (preprocess-args args)
      ;; 预处理参数：
      ;; 1. 将 -m=value 和 --mode=value 转换为 -m value 和 --mode value
      ;; 2. 移除 -- 分隔符（argparse 不认识它）
      (let loop ((remaining args)
                 (result '()))
        (if (null? remaining)
          (reverse result)
          (let ((arg (car remaining)))
            (cond
              ((equal? arg "--")
               ;; 跳过 -- 分隔符
               (loop (cdr remaining) result)
              ) ;
              ((string-starts? arg "-m=")
               (loop (cdr remaining)
                     (cons (substring arg 3) (cons "-m" result))
               ) ;loop
              ) ;
              ((string-starts? arg "--mode=")
               (loop (cdr remaining)
                     (cons (substring arg 7) (cons "--mode" result))
               ) ;loop
              ) ;
              (else
                (loop (cdr remaining) (cons arg result))
              ) ;else
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (parse-eval-args args)
      ;; 解析 eval 命令的参数
      ;; 返回: (mode . code-expr) 或 #f
      ;; args 的第一个元素是可执行文件路径，需要跳过
      (let* ((raw-args (cdr args))  ; 跳过可执行文件路径
             ;; 预处理参数（转换 -m=value 格式）
             (all-args (preprocess-args raw-args))
             ;; 使用 argparse 解析 mode
             (parser (make-argument-parser)))
        ;; 添加 mode 参数，默认值为 "liii"
        (parser :add-argument
          '((name . "mode") (type . string) (short . "m") (default . "liii"))
        ) ;parser
        ;; 解析参数（argparse 会处理 -m/--mode）
        (parser :parse-args all-args)
        ;; 获取 mode 值
        (let* ((mode (parser :get-argument "mode"))
               ;; 手动提取代码参数
               (code-args (extract-code-args all-args))
               (code-expr (combine-code code-args)))
          (if code-expr
            (cons mode code-expr)
            #f
          ) ;if
        ) ;let*
      ) ;let*
    ) ;define

    (define (apply-mode mode)
      ;; 根据模式应用相应的导入设置
      (cond
        ((or (equal? mode "liii") (equal? mode "default"))
         (import (liii base) (liii error) (liii string))
        ) ;
        ((equal? mode "r7rs")
         (import (scheme base))
        ) ;
        (else
          ;; 未知模式，使用 liii 作为默认
          (import (liii base) (liii error) (liii string))
        ) ;else
      ) ;cond
    ) ;define

    (define (main)
      ;; 程序入口点
      ;; 返回要执行的代码字符串，或 #f 表示没有代码
      (let* ((args (command-line))
             (result (parse-eval-args args)))
        (when (not result)
          (display "Usage: gf eval [OPTIONS] <code>...")
          (newline)
          (display "")
          (newline)
          (display "Evaluate Scheme code expressions.")
          (newline)
          (display "")
          (newline)
          (display "Options:")
          (newline)
          (display "  -m, --mode MODE    Set evaluation mode: liii (default), r7rs")
          (newline)
          (display "")
          (newline)
          (display "Examples:")
          (newline)
          (display "  gf eval \"(+ 1 2)\"")
          (newline)
          (display "  gf eval -m r7rs \"(+ 1 2)\"")
          (newline)
          (display "  gf eval \"(define x 1)\" \"(+ x 2)\"")
          (newline)
        ) ;when
        (when result
          (let ((mode (car result))
                (code (cdr result)))
            (apply-mode mode)
            code
          ) ;let
        ) ;when
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
