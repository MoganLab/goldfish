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
  (export make-env env? make-atom atom?
          env-tag-name env-depth env-indent env-children env-left-line env-right-line env-value
          atom-depth atom-indent atom-left-line atom-right-line atom-value
          good-env? assert-env)
  (import (liii base)
          (liii error))
  
  (begin
    ;;; 记录类型定义
    (define-record-type env
      (%make-env tag-name depth indent children left-line right-line value)
      env?
      (tag-name env-tag-name)
      (depth env-depth)
      (indent env-indent)
      (children env-children)
      (left-line env-left-line)
      (right-line env-right-line)
      (value env-value))

    ;;; atom 标记类型（用于字符串、数字等非括号结构）
    (define-record-type atom
      (%make-atom depth indent left-line right-line value)
      atom?
      (depth atom-depth)
      (indent atom-indent)
      (left-line atom-left-line)
      (right-line atom-right-line)
      (value atom-value))

    ;;; 使用具名参数构造 atom
    (define* (make-atom (depth 0) (indent -1) (left-line 0) (right-line 0) (value #f))
      ;;; 参数校验
      (when (not (integer? depth))
        (value-error "make-atom in liii/goldfmt-record: depth must be an integer"))
      (when (< depth 0)
        (value-error "make-atom in liii/goldfmt-record: depth must be non-negative"))
      (when (not (integer? indent))
        (value-error "make-atom in liii/goldfmt-record: indent must be an integer"))
      (when (< indent -1)
        (value-error "make-atom in liii/goldfmt-record: indent must be >= -1"))
      (when (not (integer? left-line))
        (value-error "make-atom in liii/goldfmt-record: left-line must be an integer"))
      (when (< left-line 0)
        (value-error "make-atom in liii/goldfmt-record: left-line must be non-negative"))
      (when (not (integer? right-line))
        (value-error "make-atom in liii/goldfmt-record: right-line must be an integer"))
      (when (< right-line 0)
        (value-error "make-atom in liii/goldfmt-record: right-line must be non-negative"))
      (%make-atom depth indent left-line right-line value))

    ;;; 使用具名参数的构造器
    (define* (make-env (tag-name "") (depth 0) (indent -1) (children (vector)) (left-line 0) (right-line 0) (value #f))
      ;;; 参数校验
      (when (not (or (string? tag-name) (eq? tag-name #f)))
        (value-error "make-env in liii/goldfmt-record: tag-name must be a string or #f"))
      (when (not (integer? depth))
        (value-error "make-env in liii/goldfmt-record: depth must be an integer"))
      (when (< depth 0)
        (value-error "make-env in liii/goldfmt-record: depth must be non-negative"))
      (when (not (integer? indent))
        (value-error "make-env in liii/goldfmt-record: indent must be an integer"))
      (when (< indent -1)
        (value-error "make-env in liii/goldfmt-record: indent must be >= -1"))
      (when (not (or (vector? children) (eq? children #f)))
        (value-error "make-env in liii/goldfmt-record: children must be a vector or #f"))
      (when (not (integer? left-line))
        (value-error "make-env in liii/goldfmt-record: left-line must be an integer"))
      (when (< left-line 0)
        (value-error "make-env in liii/goldfmt-record: left-line must be non-negative"))
      (when (not (integer? right-line))
        (value-error "make-env in liii/goldfmt-record: right-line must be an integer"))
      (when (< right-line 0)
        (value-error "make-env in liii/goldfmt-record: right-line must be non-negative"))
      (%make-env tag-name depth indent children left-line right-line value))

    ;;; 辅助函数：检查单个 env 的基本约束（不包括子环境）
    (define (env-valid? env parent-depth parent-indent)
      (and
        ; 子环境的深度必须等于父环境深度 + 1
        (= (env-depth env) (+ parent-depth 1))
        ; 根节点（depth=0）的特殊检查
        (if (= (env-depth env) 0)
            ; 根节点缩进必须为0或-1
            (or (= (env-indent env) 0)
                (= (env-indent env) -1))
            ; 非根节点：缩进必须 >= 父节点缩进
            (>= (env-indent env) parent-indent))))

    ;;; 递归检查 env 及其所有子环境
    (define (check-env env parent-depth parent-indent)
      (if (not (env-valid? env parent-depth parent-indent))
          #f
          (let ((children (env-children env))
                (my-depth (env-depth env))
                (my-indent (env-indent env)))
            (let loop ((i 0))
              (if (>= i (vector-length children))
                  #t
                  (if (check-env (vector-ref children i) my-depth my-indent)
                      (loop (+ i 1))
                      #f))))))

    ;;; good-env? 谓词：检查 env 是否满足所有校验规则
    (define (good-env? env)
      (check-env env -1 0))

    ;;; assert-env 函数：校验 env，失败抛出 value-error
    (define (assert-env env)
      (if (not (good-env? env))
          (value-error "Invalid env structure")
          #t))
    ))
