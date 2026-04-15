(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; flatten 函数测试
;;
;; 展平嵌套列表到指定深度，或者展平到最深层级。
;;
;; 语法
;; ----
;; (flatten lst)
;; (flatten lst depth)
;;
;; 参数
;; ----
;; lst : list
;; 被展平的列表。
;;
;; depth : integer 或 'deepest
;; 展平深度。当为整数时表示展平的层数，当为 'deepest 时表示展平到最深层级。
;;
;; 返回值
;; ------
;; list
;; 展平后的列表。如果指定 depth，将按指定深度展平嵌套结构。
;; 如果指定 'deepest，将完全展平所有嵌套层级。
;;
;; 说明
;; ----
;; flatten 是 SRFI-1 中的一个实用工具函数，用于将嵌套列表结构展平到单一维度的列表。
;; 它提供了灵活的展平选项：
;; - 不带 depth 参数时，默认为深度 1，即展平第一层嵌套
;; - 提供整数 depth 参数时，按指定深度展平多级嵌套结构
;; - 提供符号 'deepest 时，完全展平所有嵌套层级，无论多深
;;
;; 使用场景
;; --------
;; - 处理树形结构数据，将其转换为线性序列
;; - 清理嵌套的输入数据
;; - 字符串处理，展平嵌套字符列表
;;
;; 错误处理
;; --------
;; type-error 当 depth 参数既不是整数也不是 'deepest 符号时抛出。
;;
;; 例子
;; ----
;; (flatten '((a) () (b ()) () (c)))       => '(a b () c)
;; (flatten '((a) () (b ()) () (c)) 2)     => '(a b c)
;; (flatten '((a) () (b ()) () (c)) 'deepest) => '(a b c)
;; (flatten '((a b) c (((d)) e)) 1)        => '(a b c ((d)) e)
;; (flatten '((a b) c (((d)) e)) 3)        => '(a b c d e)


(check (flatten '((a) () (b ()) () (c))
         'deepest
       ) ;flatten
  =>
  '(a b c)
) ;check
(check (flatten '((a b) c (((d)) e)) 'deepest)
  =>
  '(a b c d e)
) ;check
(check (flatten '(a b (() (c))) 'deepest)
  =>
  '(a b c)
) ;check


(check (flatten '((a) () (b ()) () (c)) 0)
  =>
  '((a) () (b ()) () (c))
) ;check
(check (flatten '((a) () (b ()) () (c)) 1)
  =>
  '(a b () c)
) ;check
(check (flatten '((a) () (b ()) () (c)))
  =>
  '(a b () c)
) ;check
(check (flatten '((a) () (b ()) () (c)) 2)
  =>
  '(a b c)
) ;check
(check (flatten '((a) () (b ()) () (c)) -1)
  =>
  '(a b c)
) ;check
(check (flatten '((a b) c (((d)) e)) 0)
  =>
  '((a b) c (((d)) e))
) ;check
(check (flatten '((a b) c (((d)) e)) 1)
  =>
  '(a b c ((d)) e)
) ;check
(check (flatten '((a b) c (((d)) e)))
  =>
  '(a b c ((d)) e)
) ;check
(check (flatten '((a b) c (((d)) e)) 2)
  =>
  '(a b c (d) e)
) ;check
(check (flatten '((a b) c (((d)) e)) 3)
  =>
  '(a b c d e)
) ;check
(check (flatten '((a b) c (((d)) e)) -1)
  =>
  '(a b c d e)
) ;check
(check (flatten '(a b (() (c))) 0)
  =>
  '(a b (() (c)))
) ;check
(check (flatten '(a b (() (c))) 1)
  =>
  '(a b () (c))
) ;check
(check (flatten '(a b (() (c))))
  =>
  '(a b () (c))
) ;check
(check (flatten '(a b (() (c))) 2)
  =>
  '(a b c)
) ;check
(check (flatten '(a b (() (c))) -1)
  =>
  '(a b c)
) ;check


(check-catch 'type-error
  (flatten '((a) () (b ()) () (c)) 'a)
) ;check-catch
(check-catch 'type-error
  (flatten '((a) () (b ()) () (c))
    (make-vector 1 1)
  ) ;flatten
) ;check-catch


(check-report)
