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

;; (hint srfi srfi-133) - (srfi srfi-133) 的类型接口
;;
;; 本库只提供类型签名数据，不导出 (srfi srfi-133) 的任何过程，
;; 也不导入其他 hint 库，因此可以被独立加载。
;;
;; 本库实现的过程签名均在此登记；再导出自 (scheme base) 的
;; vector-copy/vector-copy! 归属 (hint scheme base)，此处不重复声明。

(define-library (hint srfi srfi-133)
  (import (scheme base))
  (export hint-srfi/srfi-133)
  (begin

    ;; 谓词
    ;; vector-empty? : (vector? -> boolean?)
    (define hint-vector-empty? '((vector? -> boolean?)))

    ;; 构造器
    ;; vector-unfold : (procedure? integer? -> vector?) 接受 0 或多个初始种子
    (define hint-vector-unfold
      '((procedure? integer? -> vector?)
        (procedure? integer? any? -> vector?)
        (procedure? integer? any? any? -> vector?)
        (procedure? integer? any? any? any? -> vector?))
    ) ;define

    ;; vector-unfold-right : 从右到左构造
    (define hint-vector-unfold-right
      '((procedure? integer? -> vector?)
        (procedure? integer? any? -> vector?)
        (procedure? integer? any? any? -> vector?)
        (procedure? integer? any? any? any? -> vector?))
    ) ;define

    ;; vector-unfold! : 原地展开
    (define hint-vector-unfold!
      '((procedure? vector? integer? integer? -> any?)
        (procedure? vector? integer? integer? any? -> any?)
        (procedure? vector? integer? integer? any? any? -> any?))
    ) ;define

    ;; vector-unfold-right! : 原地从右到左展开
    (define hint-vector-unfold-right!
      '((procedure? vector? integer? integer? -> any?)
        (procedure? vector? integer? integer? any? -> any?)
        (procedure? vector? integer? integer? any? any? -> any?))
    ) ;define

    ;; 比较器
    ;; vector= : 接受一个比较谓词与 0 或多个向量
    (define hint-vector=
      '((procedure? -> boolean?)
        (procedure? vector? -> boolean?)
        (procedure? vector? vector? -> boolean?)
        (procedure? vector? vector? vector? -> boolean?))
    ) ;define

    ;; 迭代与折叠
    ;; vector-fold : (procedure? any? vector? -> any?)
    (define hint-vector-fold '((procedure? any? vector? -> any?)))

    ;; vector-fold-right : 从右向左折叠
    (define hint-vector-fold-right '((procedure? any? vector? -> any?)))

    ;; vector-map! : (procedure? vector? ... -> vector?)
    (define hint-vector-map!
      '((procedure? vector? -> vector?)
        (procedure? vector? vector? -> vector?)
        (procedure? vector? vector? vector? -> vector?))
    ) ;define

    ;; vector-cumulate : (procedure? any? vector? -> vector?)
    (define hint-vector-cumulate '((procedure? any? vector? -> vector?)))

    ;; 谓词查询
    ;; vector-count : (procedure? vector? -> integer?)
    (define hint-vector-count '((procedure? vector? -> integer?)))

    ;; vector-any : (procedure? vector? -> any?)
    (define hint-vector-any '((procedure? vector? -> any?)))

    ;; vector-every : (procedure? vector? -> boolean?)
    (define hint-vector-every '((procedure? vector? -> boolean?)))

    ;; 查找与搜索
    ;; vector-index : 未找到返回 #f
    (define hint-vector-index '((procedure? vector? -> (or #f integer?))))

    ;; vector-index-right : 从右查找，未找到返回 #f
    (define hint-vector-index-right '((procedure? vector? -> (or #f integer?))))

    ;; vector-skip : 跳过满足谓词的元素，未找到返回 #f
    (define hint-vector-skip '((procedure? vector? -> (or #f integer?))))

    ;; vector-skip-right : 从右跳过，未找到返回 #f
    (define hint-vector-skip-right '((procedure? vector? -> (or #f integer?))))

    ;; vector-binary-search : 二分查找，可选 start/end，未找到返回 #f
    (define hint-vector-binary-search
      '((vector? any? procedure? -> (or #f integer?))
        (vector? any? procedure? integer? -> (or #f integer?))
        (vector? any? procedure? integer? integer? -> (or #f integer?)))
    ) ;define

    ;; 向量操作与连接
    ;; vector-concatenate : (list? -> vector?)
    (define hint-vector-concatenate '((list? -> vector?)))

    ;; vector-partition : (procedure? vector? -> any?) 返回 (values ret cnt)
    (define hint-vector-partition '((procedure? vector? -> any?)))

    ;; vector-append-subvectors : 接受 (vec start end) 三元组序列
    (define hint-vector-append-subvectors
      '((-> vector?)
        (vector? integer? integer? -> vector?)
        (vector? integer? integer? vector? integer? integer? -> vector?))
    ) ;define

    ;; 变异与反转
    ;; vector-swap! : 交换两个索引处的元素
    (define hint-vector-swap! '((vector? integer? integer? -> any?)))

    ;; vector-reverse! : 原地反转，可选 start/end
    (define hint-vector-reverse!
      '((vector? -> any?)
        (vector? integer? -> any?)
        (vector? integer? integer? -> any?))
    ) ;define

    ;; vector-reverse-copy : 反转拷贝，可选 start/end
    (define hint-vector-reverse-copy
      '((vector? -> vector?)
        (vector? integer? -> vector?)
        (vector? integer? integer? -> vector?))
    ) ;define

    ;; vector-reverse-copy! : 反转拷贝到目标向量，可选 start/end
    (define hint-vector-reverse-copy!
      '((vector? integer? vector? -> any?)
        (vector? integer? vector? integer? -> any?)
        (vector? integer? vector? integer? integer? -> any?))
    ) ;define

    ;; 转换
    ;; reverse-vector->list : 向量反转为列表，可选 start/end
    (define hint-reverse-vector->list
      '((vector? -> list?)
        (vector? integer? -> list?)
        (vector? integer? integer? -> list?))
    ) ;define

    ;; reverse-list->vector : 列表反转为向量
    (define hint-reverse-list->vector '((list? -> vector?)))

    (define (hint-srfi/srfi-133 name)
      (case name
       ((vector-empty?) hint-vector-empty?)
       ((vector-unfold) hint-vector-unfold)
       ((vector-unfold-right) hint-vector-unfold-right)
       ((vector-unfold!) hint-vector-unfold!)
       ((vector-unfold-right!) hint-vector-unfold-right!)
       ((vector=) hint-vector=)
       ((vector-fold) hint-vector-fold)
       ((vector-fold-right) hint-vector-fold-right)
       ((vector-map!) hint-vector-map!)
       ((vector-cumulate) hint-vector-cumulate)
       ((vector-count) hint-vector-count)
       ((vector-any) hint-vector-any)
       ((vector-every) hint-vector-every)
       ((vector-index) hint-vector-index)
       ((vector-index-right) hint-vector-index-right)
       ((vector-skip) hint-vector-skip)
       ((vector-skip-right) hint-vector-skip-right)
       ((vector-binary-search) hint-vector-binary-search)
       ((vector-concatenate) hint-vector-concatenate)
       ((vector-partition) hint-vector-partition)
       ((vector-append-subvectors) hint-vector-append-subvectors)
       ((vector-swap!) hint-vector-swap!)
       ((vector-reverse!) hint-vector-reverse!)
       ((vector-reverse-copy) hint-vector-reverse-copy)
       ((vector-reverse-copy!) hint-vector-reverse-copy!)
       ((reverse-vector->list) hint-reverse-vector->list)
       ((reverse-list->vector) hint-reverse-list->vector)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
