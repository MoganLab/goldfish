(import (liii check)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; os-type
;; 返回当前操作系统的类型字符串。
;;
;; 语法
;; ----
;; (os-type)
;;
;; 返回值
;; -----
;; string?
;; 返回操作系统类型的字符串，如 "Linux"、"Darwin"、"Windows" 等。
;;
;; 说明
;; ----
;; 1. 在 Linux 系统上返回 "Linux"
;; 2. 在 macOS 系统上返回 "Darwin"
;; 3. 在 Windows 系统上返回 "Windows"

;;; 基本功能测试
(when (os-linux?)
  (check (os-type) => "Linux")
) ;when

(when (os-macos?)
  (check (os-type) => "Darwin")
) ;when

(when (os-windows?)
  (check (os-type) => "Windows")
) ;when

(check-report)
