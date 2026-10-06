(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; with-ellipsis
;; 在局部语法模板中指定自定义标识符作为省略号关键字（目前尚未实现，调用抛出 unimplemented 错误）。
;;
;; 语法
;; ----
;; (with-ellipsis ellipsis-id body ...)
;;
;; 参数
;; ----
;; ellipsis-id : identifier
;; 用作省略号的替代标识符。
;;
;; body ... : expressions
;; 在自定义省略号作用域内求值的表达式序列。
;;
;; 返回值
;; -----
;; any
;; 最后一个 body 表达式的求值结果。

;; 1. 当前尚未支持自定义省略号，预期抛出 unimplemented 错误
(check-catch 'unimplemented
  (with-ellipsis ::: 'dummy))

(check-report)
