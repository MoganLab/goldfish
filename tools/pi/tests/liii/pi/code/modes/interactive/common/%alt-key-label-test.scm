;; %alt-key-label
;; 返回当前平台对应的 Alt/Option 修饰键标签。
;;
;; 语法
;; ----
;; (%alt-key-label)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; string?
;; macOS 下返回 "Option"，其余平台（Linux/Windows 等）返回 "Alt"。

(import (scheme base)
  (liii check)
  (liii os)
  (liii pi code modes interactive common)
) ;import

(check-set-mode! 'report)

(if (os-macos?)
  (check (%alt-key-label) => "Option")
  (check (%alt-key-label) => "Alt")
) ;if

(check-report)
