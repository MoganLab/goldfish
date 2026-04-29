(import (liii check) (liii unicode) (liii base))


(check-set-mode! 'report-failed)


;; codepoint->hexstr
;; 将 Unicode 码点转换为纯十六进制字符串。
;;
;; 语法
;; ----
;; (codepoint->hexstr codepoint)
;;
;; 参数
;; ----
;; codepoint : integer
;; Unicode 码点值。
;;
;; 返回值
;; ----
;; string
;; 纯十六进制字符串（大写，无 "U+" 或 "0x" 前缀，无固定宽度填充）。
;;
;; 描述
;; ----
;; 将 Unicode 码点转换为十六进制字符串表示。
;; 输出不包含前缀，字母为大写，不进行零填充。
;;
;; 错误处理
;; ----
;; value-error 当码点超出 Unicode 范围时。
;; type-error 当参数不是整数时。


;; 基本 ASCII 字符
(check (codepoint->hexstr 72) => "48")
(check (codepoint->hexstr 101) => "65")
(check (codepoint->hexstr 108) => "6C")
(check (codepoint->hexstr 111) => "6F")


;; 中文字符
(check (codepoint->hexstr 20013) => "4E2D")
(check (codepoint->hexstr 25991) => "6587")


;; 表情符号（辅助平面字符）
(check (codepoint->hexstr 128077) => "1F44D")
(check (codepoint->hexstr 128640) => "1F680")
(check (codepoint->hexstr 127881) => "1F389")


;; 边界值
(check (codepoint->hexstr 0) => "0")
(check (codepoint->hexstr 1114111) => "10FFFF")


;; 与 hexstr->codepoint 互逆操作
(check (codepoint->hexstr (hexstr->codepoint "48")) => "48")
(check (codepoint->hexstr (hexstr->codepoint "4E2D")) => "4E2D")
(check (codepoint->hexstr (hexstr->codepoint "1F44D")) => "1F44D")


;; 错误处理
(check-catch 'value-error (codepoint->hexstr -1))
(check-catch 'value-error (codepoint->hexstr 1114112))
(check-catch 'type-error (codepoint->hexstr "not-an-integer"))
(check-catch 'type-error (codepoint->hexstr #\A))


(check-report)
