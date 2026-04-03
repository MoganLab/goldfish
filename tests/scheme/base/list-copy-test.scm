(import (liii check)
        (scheme base)
) ;import

(check-set-mode! 'report-failed)

#|
list-copy

创建一个新列表，它是输入列表的浅拷贝。

语法
----
(list-copy list)

参数
----
list - 要复制的列表

返回值
------
返回一个新的列表，具有与输入列表相同的元素，但这是一个不同的对象。

描述
----
list-copy 函数创建输入列表的一个浅拷贝。新列表的顶层节点是新的，
但列表中的元素本身不会被复制（浅拷贝）。这使得修改原始列表不会影响拷贝的列表，
但需要注意嵌套列表的深层结构不会被复制。

该函数在 (srfi srfi-1) 模块中实现，并由 (liii list) 重新导出。

示例
----
; 基本列表复制
(list-copy '(1 2 3))      => (1 2 3)
(list-copy '())           => ()

边界条件
--------
- 空列表参数返回空列表
- 非列表参数会触发类型错误异常
- 嵌套列表的子列表为同一引用（浅拷贝特性）

时间和空间复杂度
----------------
- 时间复杂度：O(n)，其中 n 是列表长度
- 空间复杂度：O(n)，需要创建新的列表节点

|#

;; list-copy tests

;; 基本功能测试
(check (list-copy '()) => '())
(check (list-copy '(1 2 3 4 5)) => '(1 2 3 4 5))
(check (list-copy '(a b c d)) => '(a b c d))
(check (list-copy '((1 2) (3 4) (5 6))) => '((1 2) (3 4) (5 6)))

;; 空列表边界条件
(check (list-copy '()) => '())

;; 对象独立性验证 - 确保是浅拷贝
(check-false (eq? (list-copy '(1 2 3)) '(1 2 3)))

;; 突变隔离测试 - 验证列表节点独立性
(let ((orig '(a b c))
      (copy (list-copy '(a b c))))
  (check orig => copy)
  (check-false (eq? orig copy))
  ;; 验证浅拷贝特性
  (let ((mut-copy (list-copy orig)))
    (set-car! mut-copy 'x)
    (check orig => '(a b c))      ; 原始列表不受影响
    (check mut-copy => '(x b c)) ; 拷贝列表已改变
  ) ;let
) ;let

(check-report)

(check-report)
