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

(define-library (liii goldfmt-record)
  (export make-env
    env?
    make-atom
    atom?
    env-tag-name
    env-depth
    env-indent
    env-children
    env-left-line
    env-right-line
    env-value
    atom-depth
    atom-indent
    atom-left-line
    atom-right-line
    atom-value
    make-raw-string-literal
    raw-string-literal?
    raw-string-literal-source
    raw-string-literal-value
    make-char-literal
    char-literal?
    char-literal-source
    char-literal-value
    good-env?
    assert-env
  ) ;export
  (import (liii base) (liii error))

  (begin
    ;; ; 记录类型定义
    (define-record-type env
      (%make-env tag-name depth indent children left-line right-line value)
      env?
      (tag-name env-tag-name)
      (depth env-depth)
      (indent env-indent)
      (children env-children)
      (left-line env-left-line)
      (right-line env-right-line)
      (value env-value)
    ) ;define-record-type

    ;; ; atom 标记类型（用于字符串、数字等非括号结构）
    (define-record-type atom
      (%make-atom depth indent left-line right-line value)
      atom?
      (depth atom-depth)
      (indent atom-indent)
      (left-line atom-left-line)
      (right-line atom-right-line)
      (value atom-value)
    ) ;define-record-type

    ;; ; 使用具名参数构造 atom
    (define* (make-atom (depth 0) (indent -1) (left-line 0) (right-line 0) (value #f))
      (%make-atom depth indent left-line right-line value)
    ) ;define*

    (define-record-type raw-string-literal
      (%make-raw-string-literal source value)
      raw-string-literal?
      (source raw-string-literal-source)
      (value raw-string-literal-value)
    ) ;define-record-type

    (define* (make-raw-string-literal (source "") (value ""))
      (%make-raw-string-literal source value)
    ) ;define*

    (define-record-type char-literal
      (%make-char-literal source value)
      char-literal?
      (source char-literal-source)
      (value char-literal-value)
    ) ;define-record-type

    (define* (make-char-literal (source "") (value #\space))
      (%make-char-literal source value)
    ) ;define*

    ;; ; 使用具名参数的构造器
    (define* (make-env (tag-name "")
               (depth 0)
               (indent -1)
               (children (vector))
               (left-line 0)
               (right-line 0)
               (value #f)
             ) ;make-env
      (%make-env tag-name depth indent children left-line right-line value)
    ) ;define*

    ;; ; 辅助函数：检查单个 env 的基本约束（不包括子环境）
    (define (env-valid? env parent-depth parent-indent)
      (and (= (env-depth env) (+ parent-depth 1))
        (if (= (env-depth env) 0)
          (or (= (env-indent env) 0) (= (env-indent env) -1))
          (>= (env-indent env) parent-indent)
        ) ;if
      ) ;and
    ) ;define

    ;; ; 递归检查 env 及其所有子环境
    (define (check-env env parent-depth parent-indent)
      (if (not (env-valid? env parent-depth parent-indent))
        #f
        (let ((children (env-children env))
              (my-depth (env-depth env))
              (my-indent (env-indent env))
             ) ;
          (let loop
            ((i 0))
            (if (>= i (vector-length children))
              #t
              (if (check-env (vector-ref children i) my-depth my-indent) (loop (+ i 1)) #f)
            ) ;if
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    ;; ; good-env? 谓词：检查 env 是否满足所有校验规则
    (define (good-env? env)
      (check-env env -1 0)
    ) ;define

    ;; ; assert-env 函数：校验 env，失败抛出 value-error
    (define (assert-env env)
      (if (not (good-env? env)) (value-error "Invalid env structure") #t)
    ) ;define
  ) ;begin
) ;define-library
