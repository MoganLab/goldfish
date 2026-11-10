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
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; (hint liii vector) - (liii vector) 的类型接口
;;
;; 本库只提供类型签名数据，不导出 (liii vector) 的任何过程，
;; 也不导入其他 hint 库，因此可以被独立加载。
;;
;; 本库实现及特化的过程签名均在此登记；再导出自 (srfi srfi-133) 的
;; 过程归属 (hint srfi srfi-133)，此处不重复声明。

(define-library (hint liii vector)
  (import (scheme base))
  (export hint-liii/vector)
  (begin

    ;; 过滤与判断
    ;; vector-filter : (procedure? vector? -> vector?)
    (define hint-vector-filter '((procedure? vector? -> vector?)))

    ;; vector-contains? : 检查向量是否包含指定元素，可选比较过程
    (define hint-vector-contains?
      '((vector? any? -> boolean?) (vector? any? procedure? -> boolean?))
    ) ;define

    ;; 切片提取与丢弃
    ;; vector-take : 从左侧取 n 个元素
    (define hint-vector-take '((vector? integer? -> vector?)))

    ;; vector-drop : 丢弃左侧 n 个元素
    (define hint-vector-drop '((vector? integer? -> vector?)))

    ;; vector-take-right : 从右侧取 n 个元素
    (define hint-vector-take-right '((vector? integer? -> vector?)))

    ;; vector-drop-right : 丢弃右侧 n 个元素
    (define hint-vector-drop-right '((vector? integer? -> vector?)))

    ;; 特化向量：整数向量 int-vector
    ;; int-vector : 构造整数向量，接受 0 个或多个整数
    (define hint-int-vector
      '((-> vector?)
        (integer? -> vector?)
        (integer? integer? -> vector?)
        (integer? integer? integer? -> vector?)
        (integer? integer? integer? integer? -> vector?))
    ) ;define

    ;; int-vector? : 判断是否为整数向量
    (define hint-int-vector? '((any? -> boolean?)))

    ;; make-int-vector : 创建指定长度（或多维维度列表）的整数向量，可选初始填充值
    (define hint-make-int-vector
      '((integer? -> vector?)
        (integer? integer? -> vector?)
        (list? integer? -> vector?))
    ) ;define

    ;; int-vector-ref : 读取整数向量元素，支持一维及二维索引
    (define hint-int-vector-ref
      '((vector? integer? -> integer?) (vector? integer? integer? -> integer?))
    ) ;define

    ;; int-vector-set! : 设置整数向量元素，支持一维及二维索引
    (define hint-int-vector-set!
      '((vector? integer? integer? -> any?)
        (vector? integer? integer? integer? -> any?))
    ) ;define

    ;; 特化向量：浮点向量 float-vector
    ;; float-vector : 构造浮点向量，接受 0 个或多个数字（自动转为浮点数）
    (define hint-float-vector
      '((-> vector?)
        (number? -> vector?)
        (number? number? -> vector?)
        (number? number? number? -> vector?)
        (number? number? number? number? -> vector?))
    ) ;define

    ;; float-vector? : 判断是否为浮点向量
    (define hint-float-vector? '((any? -> boolean?)))

    ;; make-float-vector : 创建指定长度（或多维维度列表）的浮点向量，可选初始填充值
    (define hint-make-float-vector
      '((integer? -> vector?)
        (integer? number? -> vector?)
        (list? number? -> vector?))
    ) ;define

    ;; float-vector-ref : 读取浮点向量元素，支持一维及二维索引
    (define hint-float-vector-ref
      '((vector? integer? -> number?) (vector? integer? integer? -> number?))
    ) ;define

    ;; float-vector-set! : 设置浮点向量元素，支持一维及二维索引
    (define hint-float-vector-set!
      '((vector? integer? number? -> any?)
        (vector? integer? integer? number? -> any?))
    ) ;define

    ;; 特化向量：复数向量 complex-vector
    ;; complex-vector : 构造复数向量，接受 0 个或多个复数/数字
    (define hint-complex-vector
      '((-> vector?)
        (number? -> vector?)
        (number? number? -> vector?)
        (number? number? number? -> vector?)
        (number? number? number? number? -> vector?))
    ) ;define

    ;; complex-vector? : 判断是否为复数向量
    (define hint-complex-vector? '((any? -> boolean?)))

    ;; make-complex-vector : 创建指定长度（或多维维度列表）的复数向量，可选初始填充值
    (define hint-make-complex-vector
      '((integer? -> vector?)
        (integer? number? -> vector?)
        (list? number? -> vector?))
    ) ;define

    ;; complex-vector-ref : 读取复数向量元素，支持一维及二维索引
    (define hint-complex-vector-ref
      '((vector? integer? -> number?) (vector? integer? integer? -> number?))
    ) ;define

    ;; complex-vector-set! : 设置复数向量元素，支持一维及二维索引
    (define hint-complex-vector-set!
      '((vector? integer? number? -> any?)
        (vector? integer? integer? number? -> any?))
    ) ;define

    ;; 共享内存子向量 subvector
    ;; subvector : (vector [start [end [dimensions]]])
    (define hint-subvector
      '((vector? -> vector?)
        (vector? integer? -> vector?)
        (vector? integer? integer? -> vector?)
        (vector? integer? integer? list? -> vector?))
    ) ;define

    ;; subvector? : 判断是否为共享内存子向量
    (define hint-subvector? '((any? -> boolean?)))

    ;; subvector-position : 获取子向量在原向量中的偏移量
    (define hint-subvector-position '((vector? -> integer?)))

    ;; subvector-vector : 获取子向量所引用的底层原向量
    (define hint-subvector-vector '((vector? -> vector?)))

    (define (hint-liii/vector name)
      (case name
       ((vector-filter) hint-vector-filter)
       ((vector-contains?) hint-vector-contains?)
       ((vector-take) hint-vector-take)
       ((vector-drop) hint-vector-drop)
       ((vector-take-right) hint-vector-take-right)
       ((vector-drop-right) hint-vector-drop-right)
       ((int-vector) hint-int-vector)
       ((int-vector?) hint-int-vector?)
       ((make-int-vector) hint-make-int-vector)
       ((int-vector-ref) hint-int-vector-ref)
       ((int-vector-set!) hint-int-vector-set!)
       ((float-vector) hint-float-vector)
       ((float-vector?) hint-float-vector?)
       ((make-float-vector) hint-make-float-vector)
       ((float-vector-ref) hint-float-vector-ref)
       ((float-vector-set!) hint-float-vector-set!)
       ((complex-vector) hint-complex-vector)
       ((complex-vector?) hint-complex-vector?)
       ((make-complex-vector) hint-make-complex-vector)
       ((complex-vector-ref) hint-complex-vector-ref)
       ((complex-vector-set!) hint-complex-vector-set!)
       ((subvector) hint-subvector)
       ((subvector?) hint-subvector?)
       ((subvector-position) hint-subvector-position)
       ((subvector-vector) hint-subvector-vector)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
