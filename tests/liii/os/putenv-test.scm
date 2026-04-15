(import (liii check) (liii os))


(check-set-mode! 'report-failed)


;; putenv
;; 设置环境变量。
;;
;; 语法
;; ----
;; (putenv key value)
;;
;; 参数
;; ----
;; key : string?
;; 环境变量的名称。
;;
;; value : string?
;; 环境变量的值。
;;
;; 返回值
;; -----
;; boolean?
;; 成功返回 #t。
;;
;; 错误
;; ----
;; type-error
;; 当 key 或 value 不是字符串时抛出错误。


;; ; 基本功能测试
(check-true (putenv "TEST_VAR" "123"))
(check-true (putenv "TEST_VAR" "456"))


;; ; 错误测试
(check-catch 'type-error
  (putenv 123 "abc")
) ;check-catch
(check-catch 'type-error
  (putenv "ABC" 123)
) ;check-catch


(check-report)
