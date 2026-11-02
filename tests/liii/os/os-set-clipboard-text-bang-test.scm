;; os-set-clipboard-text!
;; 将文本字符串写入操作系统剪贴板（当前仅支持 Windows 原生剪贴板 API）。
;;
;; 语法
;; ----
;; (os-set-clipboard-text! text)
;;
;; 参数
;; ----
;; text : string?
;; 要写入剪贴板的字符串。
;;
;; 返回值
;; -----
;; boolean?
;; 成功返回 #t；若不支持或写入失败返回 #f。
;;
;; 错误
;; ----
;; type-error
;; 当 text 不是字符串时抛出错误。

(import (scheme base) (liii check) (liii os))

(check-set-mode! 'report-failed)

;; 类型检查
(check-catch 'type-error (os-set-clipboard-text! 123))
(check-catch 'type-error (os-set-clipboard-text! #f))
(check-catch 'type-error (os-set-clipboard-text! 'symbol))

;; 功能测试：返回布尔值，在 Windows 平台应返回 #t
(check-true (boolean? (os-set-clipboard-text! "hello")))
(check-true (boolean? (os-set-clipboard-text! "你好，Goldfish 剪贴板")))
(check-true (boolean? (os-set-clipboard-text! "")))

(when (os-windows?)
  (check-true (os-set-clipboard-text! "你好，Goldfish 剪贴板"))
) ;when

(check-report)
