(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; last-pair 函数测试
;;
;; 返回列表的最后一个点对。
;;
;; 语法
;; ----
;; (last-pair lst)
;;
;; 参数
;; ----
;; lst : pair?
;; 非空列表或点对结构。
;;
;; 返回值
;; ------
;; pair
;; 返回列表的最后一个点对（cons cell）。对于proper list，返回最后一个元素的点对；
;; 对于dotted list，返回包含最后元素的点对。
;;
;; 说明
;; ----
;; last-pair函数是SRFI-1规范中的选择器函数，用于获取列表的最后一个点对。
;; 该函数会遍历整个列表直到最后一个点对，无论是proper list还是dotted list都能正确处理。
;;
;; 对于proper list，返回的点对包含最后一个元素作为car，空列表作为cdr。
;; 对于dotted list，返回的点对包含最后一个元素作为car，非列表对象作为cdr。
;;
;; 使用场景
;; --------
;; - 当需要操作列表的最后一个节点时使用
;; - 在修改列表结构时，特别是添加元素到末尾
;; - 与dotted list交互时获取最终的点对结构
;;
;; 边界情况
;; --------
;; - 单元素列表返回该元素的单点对
;; - 点为参数直接返回该点对本身
;; - 空列表参数会触发错误
;;
;; 错误处理
;; --------
;; wrong-type-arg 当应用于空列表或参数不是点对/列表时抛出。


(check (last-pair '(c)) => '(c))
(check (last-pair '(b . c)) => '(b . c))


(check (last-pair '(a b c)) => '(c))
(check (last-pair '(1 2 3 4 5)) => '(5))
(check (last-pair '(a)) => '(a))


(check (last-pair '(x y z)) => '(z))
(check (last-pair '(42)) => '(42))
(check (last-pair '(() a b)) => '(b))


(check (last-pair '(a b . c))
  =>
  '(b . c)
) ;check
(check (last-pair '(a . b)) => '(a . b))
(check (last-pair '(a b c . d))
  =>
  '(c . d)
) ;check


(check (last-pair '((a b) (c d)))
  =>
  '((c d))
) ;check
(check (last-pair '((a) b (c d)))
  =>
  '((c d))
) ;check
(check (last-pair '(a (b c) d)) => '(d))


(check (last-pair '(a (b (c))))
  =>
  '((b (c)))
) ;check
(check (last-pair '("hello" "world" "test"))
  =>
  '("test")
) ;check
(check (last-pair '(1 "a" b)) => '(b))


(check (last-pair '(#t #f #t)) => '(#t))
(check (last-pair '(42 43 44.5))
  =>
  '(44.5)
) ;check
(check (last-pair '(() [] {})) => '({}))


(check-catch 'wrong-type-arg
  (last-pair '())
) ;check-catch
(check-catch 'wrong-type-arg
  (last-pair 'not-a-list)
) ;check-catch


(check-report)
