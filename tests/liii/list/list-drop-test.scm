(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; list-drop 函数测试
;;
;; Scala风格的drop函数，对越界情况容忍。
;;
;; 语法
;; ----
;; (list-drop lst n)
;;
;; 参数
;; ----
;; lst : list?
;; 源列表，从中删除元素。
;;
;; n : integer?
;; 要从开头删除的元素数量。
;;
;; 说明
;; ----
;; 与SRFI-1的drop不同，list-drop对越界情况容忍：
;; - 当n < 0时，返回原列表
;; - 当n >= 列表长度时，返回空列表
;; - 否则返回去掉前n个元素后的列表
;;
;; 错误处理
;; --------
;; type-error 当lst不是列表或n不是整数时抛出。
;;
;; 示例
;; ----
;; (list-drop '(1 2 3 4 5) 3) => '(4 5)
;; (list-drop '(1 2 3 4 5) 0) => '(1 2 3 4 5)
;; (list-drop '(1 2 3 4 5) 5) => '()
;; (list-drop '(1 2 3) -1) => '(1 2 3)
;; (list-drop '(1 2 3) 10) => '()


(check (list-drop '(1 2 3 4 5) 3)
  =>
  '(4 5)
) ;check
(check (list-drop '(1 2 3 4 5) 0)
  =>
  '(1 2 3 4 5)
) ;check
(check (list-drop '(1 2 3 4 5) 5)
  =>
  '()
) ;check


(check (list-drop '(1 2 3) -1)
  =>
  '(1 2 3)
) ;check
(check (list-drop '(1 2 3) 10) => '())


(check (list-drop '() 0) => '())
(check (list-drop '() 5) => '())


(check-catch 'type-error
  (list-drop "not a list" 2)
) ;check-catch
(check-catch 'type-error
  (list-drop '(1 2 3) "not a number")
) ;check-catch


(check-report)
