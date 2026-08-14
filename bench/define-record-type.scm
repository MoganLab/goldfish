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
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See
;; the License for the specific language governing permissions and
;; limitations under the License.
;;

;; define-record-type 性能基准测试
;; 对比旧 Scheme 宏（define-macro，已从 base.scm 移除，此处原样保留）
;; 与新的 C 宏（src/s7_liii_record.c 的 g_define_record_type）
;;
;; 两个版本生成的展开式完全相同（let 表示），因此运行时操作
;; （构造/谓词/访问/修改）性能一致，差异只在宏展开耗时
;; —— 每个使用 define-record-type 的库在加载时都要支付一次展开开销。

(import (liii timeit) (scheme base) (scheme let))

(define (bench name stmt number)
  (let ((elapsed (timeit stmt '() number)))
    (display name)
    (display ": ")
    (display elapsed)
    (display " 秒 (")
    (display number)
    (display " 次)\n")
  ) ;let
) ;define

;; 旧 Scheme 实现（从 goldfish/scheme/base.scm 原样保留，用于性能对比）
(define-macro (define-record-type-old type make ? . fields)
  (let ((obj (gensym))
        (typ (gensym))
        (args (map (lambda (field)
                     (values (list 'quote (car field))
                       (let ((par (memq (car field) (cdr make))))
                         (and (pair? par) (car par))
                       ) ;let
                     ) ;values
                   ) ;lambda
                fields
              ) ;map
        ) ;args
       ) ;
    `(begin
       (define (,? ,obj)
         (and (let? ,obj) (eq? (let-ref ,obj (quote ,typ)) (quote ,type))))
       (define ,make (inlet (quote ,typ) (quote ,type) ,@args))
       ,@(map (lambda (field)
                (when (pair? field)
                  (if (null? (cdr field))
                    (values)
                    (if (null? (cddr field))
                      `(define (,(cadr field) ,obj)
                         (let-ref ,obj (quote ,(car field))))
                      `(begin
                         (define (,(cadr field) ,obj)
                           (let-ref ,obj (quote ,(car field))))
                         (define (,(caddr field) ,obj val)
                           (let-set! ,obj (quote ,(car field)) val)))))))
           fields)
       (quote ,type))
  ) ;let
) ;define-macro

;; 一个典型的记录类型定义（4 个字段，含访问器和修改器）

(define (run-benchmarks)
  (display "=== define-record-type 宏展开性能（旧 Scheme 宏 vs 新 C 宏）===\n\n"
  ) ;display
  (bench "旧 Scheme 宏展开"
    (lambda ()
      (macroexpand (define-record-type-old :person
                     (make-person name age)
                     person?
                     (name get-name set-name!)
                     (age get-age set-age!)
                     (email get-email)
                     (phone get-phone)
                   ) ;define-record-type-old
      ) ;macroexpand
    ) ;lambda
    10000
  ) ;bench
  (bench "新 C 宏展开     "
    (lambda ()
      (macroexpand (define-record-type :person
                     (make-person name age)
                     person?
                     (name get-name set-name!)
                     (age get-age set-age!)
                     (email get-email)
                     (phone get-phone)
                   ) ;define-record-type
      ) ;macroexpand
    ) ;lambda
    10000
  ) ;bench
  (newline)

  ;; 两个版本展开式相同，运行时性能一致（展示用，各定义一个类型）
  (display "=== 运行时操作（两版展开式相同，性能一致）===\n\n")
  (eval '(define-record-type-old :pare/old
           (kons/old x y)
           pare/old?
           (x kar/old)
           (y kdr/old))
  ) ;eval
  (eval '(define-record-type :pare (kons x y) pare? (x kar) (y kdr)))
  (bench "构造（旧展开式）" (lambda () (kons/old 1 2)) 100000)
  (bench "构造（新展开式）" (lambda () (kons 1 2)) 100000)
  (let ((old-obj (kons/old 1 2)) (new-obj (kons 1 2)))
    (bench "访问（旧展开式）" (lambda () (kar/old old-obj)) 100000)
    (bench "访问（新展开式）" (lambda () (kar new-obj)) 100000)
  ) ;let
) ;define

(run-benchmarks)
