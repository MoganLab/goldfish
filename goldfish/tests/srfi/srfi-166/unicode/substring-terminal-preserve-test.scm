;; substring-terminal-preserve
;; 提取字符串中的 ANSI 转义序列与非本地控制标记，丢弃可见字符。
;;
;; 语法
;; ----
;; (substring-terminal-preserve str)
;;
;; 参数
;; ----
;; str : string?
;; 待提取的字符串。
;;
;; 返回值
;; ----
;; string?
;; 由 ANSI 颜色转义序列组成的字符串，可见字符全部丢弃。
;;
;; 说明
;; ----
;; 配合 substring-terminal-width 使用：截取可视子串后，
;; 用本函数补回原有的颜色状态，保持终端着色连续。

(import (liii check) (srfi srfi-166 unicode))

(check-set-mode! 'report-failed)

;; 无转义序列时为空串
(check (substring-terminal-preserve "abc") => "")
(check (substring-terminal-preserve "") => "")

;; 保留 ANSI 颜色转义，丢弃可见字符
(check (substring-terminal-preserve "\x1b;[31mabc\x1b;[0m")
  =>
  "\x1b;[31m\x1b;[0m"
) ;check

(check (substring-terminal-preserve "\x1b;[31mred") => "\x1b;[31m")

(check-report)
