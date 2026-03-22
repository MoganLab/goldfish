;
; Copyright (C) 2026 The Goldfish Scheme Authors
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

(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;;;
;;; Constructors
;;;

;;
;; fxmapping
;; 创建一个新的整数映射（fxmapping）。
;;
;; 语法
;; ----
;; (fxmapping key value ...)
;;
;; 参数
;; ----
;; key : exact-integer
;; 整数键。
;;
;; value : any
;; 关联的值。
;;
;; 返回值
;; -----
;; 返回包含指定键值对的新 fxmapping。
;;
(check-true (fxmapping? (fxmapping 0 'a 1 'b)))
(check (fxmapping-ref (fxmapping 0 'a 1 'b) 0 (lambda () 'not-found)) => 'a)
(check (fxmapping-ref (fxmapping 0 'a 1 'b) 1 (lambda () 'not-found)) => 'b)
(check (fxmapping-ref (fxmapping 0 'a 1 'b) 2 (lambda () 'not-found)) => 'not-found)
(check-true (fxmapping-empty? (fxmapping)))

;;
;; alist->fxmapping
;; 从关联列表（alist）创建整数映射。
;;
;; 语法
;; ----
;; (alist->fxmapping alist)
;;
;; 参数
;; ----
;; alist : list of pairs
;; 形如 ((key . value) ...) 的关联列表。
;;
;; 返回值
;; -----
;; 返回包含 alist 中所有键值对的新 fxmapping。
;; 如果存在重复键，后面的值会覆盖前面的值。
;;
(check (fxmapping-ref (alist->fxmapping '((0 . a) (1 . b))) 0 (lambda () 'not-found)) => 'a)
(check (fxmapping-ref (alist->fxmapping '((0 . a) (1 . b))) 1 (lambda () 'not-found)) => 'b)
(check-true (fxmapping-empty? (alist->fxmapping '())))

;;
;; alist->fxmapping/combinator
;; 从关联列表创建整数映射，使用自定义合并函数处理重复键。
;;
;; 语法
;; ----
;; (alist->fxmapping/combinator combiner alist)
;;
;; 参数
;; ----
;; combiner : procedure
;; 合并函数，接收三个参数：key、new-value、old-value，
;; 返回合并后的值。
;;
;; alist : list of pairs
;; 关联列表。
;;
;; 返回值
;; -----
;; 返回包含合并后键值对的新 fxmapping。
;;
(check (fxmapping-ref (alist->fxmapping/combinator (lambda (k new old) old) '((0 . a) (0 . b))) 0 (lambda () 'not-found)) => 'a)
(check (fxmapping-ref (alist->fxmapping/combinator (lambda (k new old) new) '((0 . a) (0 . b))) 0 (lambda () 'not-found)) => 'b)

;;
;; fxmapping-unfold
;; 使用 unfold 模式创建整数映射。
;;
;; 语法
;; ----
;; (fxmapping-unfold stop? mapper successor seed)
;;
;; 参数
;; ----
;; stop? : procedure
;; 停止谓词。接收当前种子，返回布尔值。
;;
;; mapper : procedure
;; 映射函数。接收当前种子，返回两个值：key 和 value。
;;
;; successor : procedure
;; 后继函数。接收当前种子，返回下一个种子。
;;
;; seed : any
;; 初始种子值。
;;
;; 返回值
;; -----
;; 返回生成的 fxmapping。
;;
(check (fxmapping-ref (fxmapping-unfold (lambda (n) (> n 3))
                                        (lambda (n) (values n (* n 10)))
                                        (lambda (n) (+ n 1))
                                        0)
                      2
                      (lambda () 'not-found))
       => 20
) ;check
(check (fxmapping-ref (fxmapping-unfold (lambda (n) (> n 3))
                                        (lambda (n) (values n (* n 10)))
                                        (lambda (n) (+ n 1))
                                        0)
                      5
                      (lambda () #f))
       => #f
) ;check

;;;
;;; Predicates
;;;

;;
;; fxmapping?
;; 检查对象是否为整数映射（fxmapping）。
;;
;; 语法
;; ----
;; (fxmapping? obj)
;;
;; 参数
;; ----
;; obj : any
;; 要检查的对象。
;;
;; 返回值
;; -----
;; 如果 obj 是 fxmapping，返回 #t；否则返回 #f。
;;
(check-true (fxmapping? (fxmapping 0 'a)))
(check-false (fxmapping? '()))
(check-false (fxmapping? 42))

;;
;; fxmapping-contains?
;; 检查映射是否包含指定键。
;;
;; 语法
;; ----
;; (fxmapping-contains? fxmap key)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 要检查的键。
;;
;; 返回值
;; -----
;; 如果 fxmap 包含 key，返回 #t；否则返回 #f。
;;
(check-true (fxmapping-contains? (fxmapping 0 'a 1 'b) 0))
(check-true (fxmapping-contains? (fxmapping 0 'a 1 'b) 1))
(check-false (fxmapping-contains? (fxmapping 0 'a 1 'b) 2))

;;
;; fxmapping-empty?
;; 检查映射是否为空。
;;
;; 语法
;; ----
;; (fxmapping-empty? fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 如果 fxmap 不包含任何键值对，返回 #t；否则返回 #f。
;;
(check-true (fxmapping-empty? (fxmapping)))
(check-false (fxmapping-empty? (fxmapping 0 'a)))

;;
;; fxmapping-disjoint?
;; 检查两个映射是否不相交（没有共同键）。
;;
;; 语法
;; ----
;; (fxmapping-disjoint? fxmap1 fxmap2)
;;
;; 参数
;; ----
;; fxmap1, fxmap2 : fxmapping
;; 要比较的两个映射。
;;
;; 返回值
;; -----
;; 如果两个映射没有共同键，返回 #t；否则返回 #f。
;;
(check-true (fxmapping-disjoint? (fxmapping 0 'a) (fxmapping 1 'b)))
(check-false (fxmapping-disjoint? (fxmapping 0 'a) (fxmapping 0 'b)))
(check-true (fxmapping-disjoint? (fxmapping 0 'a 1 'b) (fxmapping 2 'c 3 'd)))

;;;
;;; Accessors
;;;

;;
;; fxmapping-ref
;; 获取指定键关联的值。
;;
;; 语法
;; ----
;; (fxmapping-ref fxmap key)
;; (fxmapping-ref fxmap key failure)
;; (fxmapping-ref fxmap key failure success)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 要查找的键。
;;
;; failure : procedure (可选)
;; 键不存在时调用的无参过程，默认抛出错误。
;;
;; success : procedure (可选)
;; 键存在时调用的单参过程，接收值并返回结果，默认为 values。
;;
;; 返回值
;; -----
;; 如果键存在，返回关联值（或 success 的结果）；
;; 如果键不存在且提供了 failure，返回 failure 的结果；
;; 否则抛出错误。
;;
(check (fxmapping-ref (fxmapping 0 'a 1 'b) 0 (lambda () 'not-found)) => 'a)
(check (fxmapping-ref (fxmapping 0 'a 1 'b) 1 (lambda () 'not-found)) => 'b)
(check (fxmapping-ref (fxmapping 0 'a 1 'b) 2 (lambda () 'not-found)) => 'not-found)
(check (fxmapping-ref (fxmapping 0 'a 1 'b) 0 (lambda () 'fail) (lambda (v) (list 'found v))) => '(found a))

;;
;; fxmapping-min
;; 获取键最小的键值对。
;;
;; 语法
;; ----
;; (fxmapping-min fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射，必须非空。
;;
;; 返回值
;; -----
;; 返回两个值：最小的键和关联的值。
;;
(let-values (((k v) (fxmapping-min (fxmapping 0 'a 1 'b 2 'c))))
  (check k => 0)
  (check v => 'a)
) ;let-values

;;
;; fxmapping-max
;; 获取键最大的键值对。
;;
;; 语法
;; ----
;; (fxmapping-max fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射，必须非空。
;;
;; 返回值
;; -----
;; 返回两个值：最大的键和关联的值。
;;
(let-values (((k v) (fxmapping-max (fxmapping 0 'a 1 'b 2 'c))))
  (check k => 2)
  (check v => 'c)
) ;let-values

;;;
;;; Updaters
;;;

;;
;; fxmapping-adjoin
;; 添加键值对到映射，不覆盖已存在的键。
;;
;; 语法
;; ----
;; (fxmapping-adjoin fxmap key value ...)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 要添加的键。
;;
;; value : any
;; 关联的值。
;;
;; 返回值
;; -----
;; 返回新的 fxmapping，包含原映射的所有键值对以及新增的键值对。
;; 如果键已存在，保留原值。
;;
(check (fxmapping-ref (fxmapping-adjoin (fxmapping 0 'a) 1 'b) 1 (lambda () 'not-found)) => 'b)
(check (fxmapping-ref (fxmapping-adjoin (fxmapping 0 'a) 0 'b) 0 (lambda () 'not-found)) => 'a)

;;
;; fxmapping-set
;; 设置键值对，覆盖已存在的键。
;;
;; 语法
;; ----
;; (fxmapping-set fxmap key value ...)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 要设置的键。
;;
;; value : any
;; 关联的值。
;;
;; 返回值
;; -----
;; 返回新的 fxmapping，包含原映射的所有键值对以及新设置的键值对。
;; 如果键已存在，新值会覆盖原值。
;;
(check (fxmapping-ref (fxmapping-set (fxmapping 0 'a) 0 'b) 0 (lambda () 'not-found)) => 'b)
(check (fxmapping-ref (fxmapping-set (fxmapping 0 'a) 1 'b) 1 (lambda () 'not-found)) => 'b)

;;
;; fxmapping-adjust
;; 调整指定键的值。
;;
;; 语法
;; ----
;; (fxmapping-adjust fxmap key proc)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 要调整的键。
;;
;; proc : procedure
;; 单参过程，接收原值并返回新值。
;;
;; 返回值
;; -----
;; 返回新的 fxmapping，指定键的值已调整。
;; 如果键不存在，返回原映射。
;;
(check (fxmapping-ref (fxmapping-adjust (fxmapping 0 10) 0 (lambda (v) (* v 2))) 0 (lambda () 'not-found)) => 20)

;;
;; fxmapping-delete
;; 删除指定键。
;;
;; 语法
;; ----
;; (fxmapping-delete fxmap key ...)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 要删除的键。
;;
;; 返回值
;; -----
;; 返回新的 fxmapping，不包含指定的键。
;;
(check-false (fxmapping-contains? (fxmapping-delete (fxmapping 0 'a 1 'b) 0) 0))
(check-true (fxmapping-contains? (fxmapping-delete (fxmapping 0 'a 1 'b) 0) 1))
(check-true (fxmapping-contains? (fxmapping-delete (fxmapping 0 'a 1 'b) 2) 0))

;;
;; fxmapping-delete-all
;; 删除多个键。
;;
;; 语法
;; ----
;; (fxmapping-delete-all fxmap keys)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; keys : list of exact-integer
;; 要删除的键列表。
;;
;; 返回值
;; -----
;; 返回新的 fxmapping，不包含 keys 中的任何键。
;;
(check-false (fxmapping-contains? (fxmapping-delete-all (fxmapping 0 'a 1 'b 2 'c) '(0 2)) 0))
(check-true (fxmapping-contains? (fxmapping-delete-all (fxmapping 0 'a 1 'b 2 'c) '(0 2)) 1))
(check-false (fxmapping-contains? (fxmapping-delete-all (fxmapping 0 'a 1 'b 2 'c) '(0 2)) 2))

;;
;; fxmapping-delete-min
;; 删除键最小的键值对。
;;
;; 语法
;; ----
;; (fxmapping-delete-min fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射，必须非空。
;;
;; 返回值
;; -----
;; 返回新的 fxmapping，不包含键最小的键值对。
;;
(let ((m (fxmapping 0 'a 1 'b 2 'c)))
  (check-false (fxmapping-contains? (fxmapping-delete-min m) 0))
  (check-true (fxmapping-contains? (fxmapping-delete-min m) 1))
) ;let

;;
;; fxmapping-delete-max
;; 删除键最大的键值对。
;;
;; 语法
;; ----
;; (fxmapping-delete-max fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射，必须非空。
;;
;; 返回值
;; -----
;; 返回新的 fxmapping，不包含键最大的键值对。
;;
(let ((m (fxmapping 0 'a 1 'b 2 'c)))
  (check-false (fxmapping-contains? (fxmapping-delete-max m) 2))
  (check-true (fxmapping-contains? (fxmapping-delete-max m) 1))
) ;let

;;
;; fxmapping-pop-min
;; 弹出键最小的键值对。
;;
;; 语法
;; ----
;; (fxmapping-pop-min fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射，必须非空。
;;
;; 返回值
;; -----
;; 返回三个值：最小的键、关联的值、以及不包含该键值对的新映射。
;;
(let-values (((k v m) (fxmapping-pop-min (fxmapping 0 'a 1 'b))))
  (check k => 0)
  (check v => 'a)
  (check-false (fxmapping-contains? m 0))
) ;let-values

;;
;; fxmapping-pop-max
;; 弹出键最大的键值对。
;;
;; 语法
;; ----
;; (fxmapping-pop-max fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射，必须非空。
;;
;; 返回值
;; -----
;; 返回三个值：最大的键、关联的值、以及不包含该键值对的新映射。
;;
(let-values (((k v m) (fxmapping-pop-max (fxmapping 0 'a 1 'b))))
  (check k => 1)
  (check v => 'b)
  (check-false (fxmapping-contains? m 1))
) ;let-values

;;;
;;; The whole fxmapping
;;;

;;
;; fxmapping-size
;; 获取映射的大小（键值对数量）。
;;
;; 语法
;; ----
;; (fxmapping-size fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回 fxmap 中键值对的数量。
;;
(check (fxmapping-size (fxmapping)) => 0)
(check (fxmapping-size (fxmapping 0 'a)) => 1)
(check (fxmapping-size (fxmapping 0 'a 1 'b 2 'c)) => 3)

;;
;; fxmapping-find
;; 查找满足谓词的第一个键值对。
;;
;; 语法
;; ----
;; (fxmapping-find pred fxmap failure)
;; (fxmapping-find pred fxmap failure success)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数，接收 key 和 value，返回布尔值。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; failure : procedure
;; 未找到时调用的无参过程。
;;
;; success : procedure (可选)
;; 找到时调用的双参过程，接收 key 和 value。
;;
;; 返回值
;; -----
;; 如果找到满足 pred 的键值对，返回 success 的结果（默认为两个值：key 和 value）；
;; 否则返回 failure 的结果。
;;
(let-values (((k v) (fxmapping-find (lambda (k v) (> k 5)) (fxmapping 3 'a 7 'b 10 'c) (lambda () (values #f #f)))))
  (check k => 7)
  (check v => 'b)
) ;let-values
(let-values (((k v) (fxmapping-find (lambda (k v) (> k 100)) (fxmapping 3 'a 7 'b) (lambda () (values #f #f)))))
  (check k => #f)
) ;let-values

;;
;; fxmapping-count
;; 统计满足谓词的键值对数量。
;;
;; 语法
;; ----
;; (fxmapping-count pred fxmap)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数，接收 key 和 value，返回布尔值。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回满足 pred 的键值对数量。
;;
(check (fxmapping-count (lambda (k v) (> k 5)) (fxmapping 3 'a 7 'b 10 'c)) => 2)
(check (fxmapping-count (lambda (k v) (symbol? v)) (fxmapping 0 'a 1 2 2 'c)) => 2)

;;
;; fxmapping-any?
;; 检查是否存在满足谓词的键值对。
;;
;; 语法
;; ----
;; (fxmapping-any? pred fxmap)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数，接收 key 和 value，返回布尔值。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 如果存在满足 pred 的键值对，返回 #t；否则返回 #f。
;;
(check-true (fxmapping-any? (lambda (k v) (> k 5)) (fxmapping 3 'a 7 'b 10 'c)))
(check-false (fxmapping-any? (lambda (k v) (> k 100)) (fxmapping 3 'a 7 'b)))

;;
;; fxmapping-every?
;; 检查是否所有键值对都满足谓词。
;;
;; 语法
;; ----
;; (fxmapping-every? pred fxmap)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数，接收 key 和 value，返回布尔值。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 如果所有键值对都满足 pred，返回 #t；否则返回 #f。
;;
(check-true (fxmapping-every? (lambda (k v) (> k 0)) (fxmapping 1 'a 2 'b 3 'c)))
(check-false (fxmapping-every? (lambda (k v) (> k 0)) (fxmapping 0 'a 1 'b)))

;;;
;;; Mapping and folding
;;;

;;
;; fxmapping-map
;; 映射函数转换所有值。
;;
;; 语法
;; ----
;; (fxmapping-map proc fxmap)
;;
;; 参数
;; ----
;; proc : procedure
;; 映射函数，接收 key 和 value，返回新值。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回新的 fxmapping，所有值都经过 proc 转换。
;;
(check (fxmapping-ref (fxmapping-map (lambda (k v) (* v 10)) (fxmapping 0 1 1 2 2 3)) 0 (lambda () 'not-found)) => 10)
(check (fxmapping-ref (fxmapping-map (lambda (k v) (* v 10)) (fxmapping 0 1 1 2 2 3)) 1 (lambda () 'not-found)) => 20)

;;
;; fxmapping-for-each
;; 遍历所有键值对执行副作用操作。
;;
;; 语法
;; ----
;; (fxmapping-for-each proc fxmap)
;;
;; 参数
;; ----
;; proc : procedure
;; 遍历函数，接收 key 和 value。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 未指定返回值（用于副作用）。
;;
(let ((result '()))
  (fxmapping-for-each (lambda (k v) (set! result (cons (cons k v) result)))
                      (fxmapping 0 'a 1 'b 2 'c)
  ) ;fxmapping-for-each
  (check (length result) => 3)
) ;let

;;
;; fxmapping-fold
;; 左折叠遍历映射（按键升序）。
;;
;; 语法
;; ----
;; (fxmapping-fold proc nil fxmap)
;;
;; 参数
;; ----
;; proc : procedure
;; 折叠函数，接收 key、value 和累积值，返回新累积值。
;;
;; nil : any
;; 初始累积值。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回最终累积值。
;;
(check (fxmapping-fold (lambda (k v acc) (+ v acc)) 0 (fxmapping 0 10 1 20 2 30)) => 60)
(check (fxmapping-fold (lambda (k v acc) (cons k acc)) '() (fxmapping 0 'a 1 'b 2 'c)) => '(2 1 0))

;;
;; fxmapping-fold-right
;; 右折叠遍历映射（按键降序）。
;;
;; 语法
;; ----
;; (fxmapping-fold-right proc nil fxmap)
;;
;; 参数
;; ----
;; proc : procedure
;; 折叠函数，接收 key、value 和累积值，返回新累积值。
;;
;; nil : any
;; 初始累积值。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回最终累积值。
;;
(check (fxmapping-fold-right (lambda (k v acc) (cons k acc)) '() (fxmapping 0 'a 1 'b 2 'c)) => '(0 1 2))

;;
;; fxmapping-map->list
;; 映射并转换为列表。
;;
;; 语法
;; ----
;; (fxmapping-map->list proc fxmap)
;;
;; 参数
;; ----
;; proc : procedure
;; 映射函数，接收 key 和 value，返回列表元素。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回 proc 应用于所有键值对的结果列表（按键降序）。
;;
(check (fxmapping-map->list (lambda (k v) (cons k v)) (fxmapping 0 'a 1 'b)) => '((0 . a) (1 . b)))

;;
;; fxmapping-filter
;; 过滤满足谓词的键值对。
;;
;; 语法
;; ----
;; (fxmapping-filter pred fxmap)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数，接收 key 和 value，返回布尔值。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回只包含满足 pred 的键值对的新 fxmapping。
;;
(let ((filtered (fxmapping-filter (lambda (k v) (> k 5)) (fxmapping 3 'a 7 'b 10 'c))))
  (check-false (fxmapping-contains? filtered 3))
  (check-true (fxmapping-contains? filtered 7))
  (check-true (fxmapping-contains? filtered 10))
) ;let

;;
;; fxmapping-remove
;; 移除满足谓词的键值对。
;;
;; 语法
;; ----
;; (fxmapping-remove pred fxmap)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数，接收 key 和 value，返回布尔值。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回不包含满足 pred 的键值对的新 fxmapping。
;;
(let ((removed (fxmapping-remove (lambda (k v) (> k 5)) (fxmapping 3 'a 7 'b 10 'c))))
  (check-true (fxmapping-contains? removed 3))
  (check-false (fxmapping-contains? removed 7))
  (check-false (fxmapping-contains? removed 10))
) ;let

;;
;; fxmapping-partition
;; 按谓词分割映射。
;;
;; 语法
;; ----
;; (fxmapping-partition pred fxmap)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数，接收 key 和 value，返回布尔值。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回两个值：满足 pred 的键值对组成的新映射，和不满足的组成的新映射。
;;
(let-values (((yes no) (fxmapping-partition (lambda (k v) (> k 5)) (fxmapping 3 'a 7 'b 10 'c))))
  (check-true (fxmapping-contains? yes 7))
  (check-false (fxmapping-contains? yes 3))
  (check-true (fxmapping-contains? no 3))
  (check-false (fxmapping-contains? no 7))
) ;let-values

;;;
;;; Conversion
;;;

;;
;; fxmapping->alist
;; 转换为关联列表（升序）。
;;
;; 语法
;; ----
;; (fxmapping->alist fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回按键升序排列的关联列表 ((key . value) ...)。
;;
(check (fxmapping->alist (fxmapping 0 'a 1 'b)) => '((0 . a) (1 . b)))
(check (fxmapping->alist (fxmapping)) => '())

;;
;; fxmapping->decreasing-alist
;; 转换为关联列表（降序）。
;;
;; 语法
;; ----
;; (fxmapping->decreasing-alist fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回按键降序排列的关联列表。
;;
(check (fxmapping->decreasing-alist (fxmapping 0 'a 1 'b)) => '((1 . b) (0 . a)))

;;
;; fxmapping-keys
;; 获取所有键（升序）。
;;
;; 语法
;; ----
;; (fxmapping-keys fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回按键升序排列的键列表。
;;
(check (fxmapping-keys (fxmapping 0 'a 1 'b 2 'c)) => '(0 1 2))

;;
;; fxmapping-values
;; 获取所有值（按键升序）。
;;
;; 语法
;; ----
;; (fxmapping-values fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回按对应键升序排列的值列表。
;;
(check (fxmapping-values (fxmapping 0 'a 1 'b 2 'c)) => '(a b c))

;;;
;;; Comparison
;;;

;;
;; fxmapping=?
;; 比较多个映射是否相等。
;;
;; 语法
;; ----
;; (fxmapping=? comparator fxmap1 fxmap2 ...)
;;
;; 参数
;; ----
;; comparator : comparator
;; 值比较器。
;;
;; fxmap1, fxmap2, ... : fxmapping
;; 要比较的映射。
;;
;; 返回值
;; -----
;; 如果所有映射包含相同的键，且对应值通过 comparator 比较相等，返回 #t；
;; 否则返回 #f。
;;
(check-true (fxmapping=? eqv? (fxmapping 0 'a) (fxmapping 0 'a)))
(check-false (fxmapping=? eqv? (fxmapping 0 'a) (fxmapping 0 'b)))
(check-true (fxmapping=? eqv? (fxmapping 0 'a) (fxmapping 0 'a) (fxmapping 0 'a)))

;;;
;;; Set theory operations
;;;

;;
;; fxmapping-union
;; 并集操作。
;;
;; 语法
;; ----
;; (fxmapping-union fxmap1 fxmap2 ...)
;;
;; 参数
;; ----
;; fxmap1, fxmap2, ... : fxmapping
;; 要合并的映射。
;;
;; 返回值
;; -----
;; 返回包含所有映射中所有键的新 fxmapping。
;; 对于重复键，后面的映射的值优先。
;;
(let ((union (fxmapping-union (fxmapping 0 'a 1 'b) (fxmapping 1 'B 2 'c))))
  (check (fxmapping-ref union 0 (lambda () 'not-found)) => 'a)
  (check (fxmapping-ref union 1 (lambda () 'not-found)) => 'B)
  (check (fxmapping-ref union 2 (lambda () 'not-found)) => 'c)
) ;let

;;
;; fxmapping-intersection
;; 交集操作。
;;
;; 语法
;; ----
;; (fxmapping-intersection fxmap1 fxmap2 ...)
;;
;; 参数
;; ----
;; fxmap1, fxmap2, ... : fxmapping
;; 要求交集的映射。
;;
;; 返回值
;; -----
;; 返回只包含在所有映射中都存在的键的新 fxmapping。
;; 对于重复键，后面的映射的值优先。
;;
(let ((intersection (fxmapping-intersection (fxmapping 0 'a 1 'b 2 'c) (fxmapping 1 'B 2 'C 3 'd))))
  (check-false (fxmapping-contains? intersection 0))
  (check (fxmapping-ref intersection 1 (lambda () 'not-found)) => 'B)
  (check (fxmapping-ref intersection 2 (lambda () 'not-found)) => 'C)
) ;let

;;
;; fxmapping-difference
;; 差集操作。
;;
;; 语法
;; ----
;; (fxmapping-difference fxmap1 fxmap2 ...)
;;
;; 参数
;; ----
;; fxmap1 : fxmapping
;; 基础映射。
;;
;; fxmap2, ... : fxmapping
;; 要减去的映射。
;;
;; 返回值
;; -----
;; 返回 fxmap1 中不包含在 fxmap2... 中的键的新 fxmapping。
;;
(let ((diff (fxmapping-difference (fxmapping 0 'a 1 'b 2 'c) (fxmapping 1 'x 3 'y))))
  (check-true (fxmapping-contains? diff 0))
  (check-false (fxmapping-contains? diff 1))
  (check-true (fxmapping-contains? diff 2))
) ;let

;;
;; fxmapping-xor
;; 对称差集操作。
;;
;; 语法
;; ----
;; (fxmapping-xor fxmap1 fxmap2)
;;
;; 参数
;; ----
;; fxmap1, fxmap2 : fxmapping
;; 要计算对称差集的映射。
;;
;; 返回值
;; -----
;; 返回只包含在恰好一个映射中存在的键的新 fxmapping。
;;
(let ((xor (fxmapping-xor (fxmapping 0 'a 1 'b) (fxmapping 1 'B 2 'c))))
  (check-true (fxmapping-contains? xor 0))
  (check-false (fxmapping-contains? xor 1))
  (check-true (fxmapping-contains? xor 2))
) ;let

;;;
;;; Subsets
;;;

;;
;; fxsubmapping=
;; 获取指定键的子映射。
;;
;; 语法
;; ----
;; (fxsubmapping= fxmap key)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 指定的键。
;;
;; 返回值
;; -----
;; 如果 key 存在于 fxmap 中，返回只包含该键值对的映射；
;; 否则返回空映射。
;;
(check (fxmapping-ref (fxsubmapping= (fxmapping 0 'a 1 'b) 0) 0 (lambda () #f)) => 'a)
(check-true (fxmapping-empty? (fxsubmapping= (fxmapping 0 'a 1 'b) 2)))

;;
;; fxmapping-open-interval
;; 获取开区间子映射 (low, high)。
;;
;; 语法
;; ----
;; (fxmapping-open-interval fxmap low high)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; low, high : exact-integer
;; 区间边界（不包含）。
;;
;; 返回值
;; -----
;; 返回只包含键在 (low, high) 范围内的键值对的新 fxmapping。
;;
(let ((m (fxmapping-open-interval (fxmapping 0 'a 1 'b 2 'c 3 'd 4 'e) 1 4)))
  (check-false (fxmapping-contains? m 0))
  (check-false (fxmapping-contains? m 1))
  (check-true (fxmapping-contains? m 2))
  (check-true (fxmapping-contains? m 3))
  (check-false (fxmapping-contains? m 4))
) ;let

;;
;; fxmapping-closed-interval
;; 获取闭区间子映射 [low, high]。
;;
;; 语法
;; ----
;; (fxmapping-closed-interval fxmap low high)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; low, high : exact-integer
;; 区间边界（包含）。
;;
;; 返回值
;; -----
;; 返回只包含键在 [low, high] 范围内的键值对的新 fxmapping。
;;
(let ((m (fxmapping-closed-interval (fxmapping 0 'a 1 'b 2 'c 3 'd 4 'e) 1 4)))
  (check-false (fxmapping-contains? m 0))
  (check-true (fxmapping-contains? m 1))
  (check-true (fxmapping-contains? m 2))
  (check-true (fxmapping-contains? m 4))
  (check-false (fxmapping-contains? m 5))
) ;let

;;
;; fxmapping-open-closed-interval
;; 获取半开半闭区间子映射 (low, high]。
;;
;; 语法
;; ----
;; (fxmapping-open-closed-interval fxmap low high)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; low, high : exact-integer
;; 区间边界（low 不包含，high 包含）。
;;
;; 返回值
;; -----
;; 返回只包含键在 (low, high] 范围内的键值对的新 fxmapping。
;;
(let ((m (fxmapping-open-closed-interval (fxmapping 0 'a 1 'b 2 'c 3 'd 4 'e) 1 4)))
  (check-false (fxmapping-contains? m 1))
  (check-true (fxmapping-contains? m 2))
  (check-true (fxmapping-contains? m 4))
) ;let

;;
;; fxmapping-closed-open-interval
;; 获取半闭半开区间子映射 [low, high)。
;;
;; 语法
;; ----
;; (fxmapping-closed-open-interval fxmap low high)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; low, high : exact-integer
;; 区间边界（low 包含，high 不包含）。
;;
;; 返回值
;; -----
;; 返回只包含键在 [low, high) 范围内的键值对的新 fxmapping。
;;
(let ((m (fxmapping-closed-open-interval (fxmapping 0 'a 1 'b 2 'c 3 'd 4 'e) 1 4)))
  (check-true (fxmapping-contains? m 1))
  (check-true (fxmapping-contains? m 3))
  (check-false (fxmapping-contains? m 4))
) ;let

;;
;; fxsubmapping<
;; 获取键小于指定值的子映射。
;;
;; 语法
;; ----
;; (fxsubmapping< fxmap key)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 边界键（不包含）。
;;
;; 返回值
;; -----
;; 返回只包含键严格小于 key 的键值对的新 fxmapping。
;;
(let ((m (fxsubmapping< (fxmapping 0 'a 1 'b 2 'c 3 'd) 2)))
  (check-true (fxmapping-contains? m 0))
  (check-true (fxmapping-contains? m 1))
  (check-false (fxmapping-contains? m 2))
) ;let

;;
;; fxsubmapping<=
;; 获取键小于等于指定值的子映射。
;;
;; 语法
;; ----
;; (fxsubmapping<= fxmap key)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 边界键（包含）。
;;
;; 返回值
;; -----
;; 返回只包含键小于等于 key 的键值对的新 fxmapping。
;;
(let ((m (fxsubmapping<= (fxmapping 0 'a 1 'b 2 'c 3 'd) 2)))
  (check-true (fxmapping-contains? m 1))
  (check-true (fxmapping-contains? m 2))
  (check-false (fxmapping-contains? m 3))
) ;let

;;
;; fxsubmapping>
;; 获取键大于指定值的子映射。
;;
;; 语法
;; ----
;; (fxsubmapping> fxmap key)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 边界键（不包含）。
;;
;; 返回值
;; -----
;; 返回只包含键严格大于 key 的键值对的新 fxmapping。
;;
(let ((m (fxsubmapping> (fxmapping 0 'a 1 'b 2 'c 3 'd) 1)))
  (check-false (fxmapping-contains? m 1))
  (check-true (fxmapping-contains? m 2))
  (check-true (fxmapping-contains? m 3))
) ;let

;;
;; fxsubmapping>=
;; 获取键大于等于指定值的子映射。
;;
;; 语法
;; ----
;; (fxsubmapping>= fxmap key)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 边界键（包含）。
;;
;; 返回值
;; -----
;; 返回只包含键大于等于 key 的键值对的新 fxmapping。
;;
(let ((m (fxsubmapping>= (fxmapping 0 'a 1 'b 2 'c 3 'd) 1)))
  (check-false (fxmapping-contains? m 0))
  (check-true (fxmapping-contains? m 1))
  (check-true (fxmapping-contains? m 2))
) ;let

;;
;; fxmapping-split
;; 按键分割映射。
;;
;; 语法
;; ----
;; (fxmapping-split fxmap key)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 分割点。
;;
;; 返回值
;; -----
;; 返回两个值：键小于 key 的映射，和键大于等于 key 的映射。
;;
(let-values (((low high) (fxmapping-split (fxmapping 0 'a 1 'b 2 'c 3 'd) 2)))
  (check-true (fxmapping-contains? low 0))
  (check-true (fxmapping-contains? low 1))
  (check-false (fxmapping-contains? low 2))
  (check-true (fxmapping-contains? high 2))
  (check-true (fxmapping-contains? high 3))
  (check-false (fxmapping-contains? high 1))
) ;let-values

(check-report)
