;; exclude-tools-from-list
;; 从工具名单中剔除被禁用的工具
;;
;; 语法
;; ----
;; (exclude-tools-from-list ...)
;;
;; 参数
;; ----
;; 见各断言场景：本测试按用例组织，覆盖典型入参。
;;
;; 返回值
;; ------
;; 见各断言场景：按被测函数实际返回值断言。

(import (liii check) (liii pi code core tools))

(check-set-mode! 'report-failed)

(check (exclude-tools-from-list '("read" "bash" "edit" "write") #f)
  =>
  '("read" "bash" "edit" "write")
) ;check
(check (exclude-tools-from-list '("read" "bash" "edit" "write") "")
  =>
  '("read" "bash" "edit" "write")
) ;check
(check (exclude-tools-from-list '("read" "bash" "edit" "write") "bash,write,edit")
  =>
  '("read")
) ;check
(check (exclude-tools-from-list '("read" "bash" "edit" "write") " bash , edit ")
  =>
  '("read" "write")
) ;check
(check (exclude-tools-from-list '("read" "bash" "edit" "write") "foo,bash")
  =>
  '("read" "edit" "write")
) ;check
(check (exclude-tools-from-list '("read" "bash") "read,bash") => '())

(check-report)
