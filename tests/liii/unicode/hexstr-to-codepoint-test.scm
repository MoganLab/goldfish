(import (liii check) (liii unicode) (liii base))


(check-set-mode! 'report-failed)


;; hexstr->codepoint
;; 将十六进制字符串转换为 Unicode 码点。
;;
;; 语法
;; ----
;; (hexstr->codepoint hexstr)
;;
;; 参数
;; ----
;; hexstr : string
;; 十六进制字符串（纯十六进制，不含 "0x" 或 "U+" 前缀，如 "48"、"4E2D"、"1F44D"）。
;;
;; 返回值
;; ----
;; integer
;; Unicode 码点。
;;
;; 描述
;; ----
;; 解析纯十六进制字符串为 Unicode 码点值。
;; 输入应为纯十六进制数字，不包含任何前缀。
;;
;; 错误处理
;; ----
;; value-error 当字符串为空、格式无效或码点超出范围时。
;; type-error 当参数不是字符串时。


;; 基本 ASCII 字符
(check (hexstr->codepoint "48") => 72)
(check (hexstr->codepoint "65") => 101)
(check (hexstr->codepoint "6C") => 108)


;; 带前导零的十六进制字符串
(check (hexstr->codepoint "0048") => 72)
(check (hexstr->codepoint "0041") => 65)
(check (hexstr->codepoint "007A") => 122)


;; 中文字符
(check (hexstr->codepoint "4E2D") => 20013)
(check (hexstr->codepoint "6587") => 25991)


;; 表情符号（辅助平面字符）
(check (hexstr->codepoint "1F44D") => 128077)
(check (hexstr->codepoint "1F680") => 128640)
(check (hexstr->codepoint "1F389") => 127881)


;; 边界值
(check (hexstr->codepoint "0") => 0)
(check (hexstr->codepoint "10FFFF") => 1114111)


;; 小写字母
(check (hexstr->codepoint "48") => 72)
(check (hexstr->codepoint "4e2d") => 20013)


;; 与 codepoint->hexstr 互逆操作
(check (hexstr->codepoint (codepoint->hexstr 72)) => 72)
(check (hexstr->codepoint (codepoint->hexstr 20013)) => 20013)
(check (hexstr->codepoint (codepoint->hexstr 128077)) => 128077)


;; 错误处理
(check-catch 'value-error (hexstr->codepoint ""))
(check-catch 'value-error (hexstr->codepoint "110000"))
(check-catch 'value-error (hexstr->codepoint "not-hex"))
(check-catch 'type-error (hexstr->codepoint 123))


(check-report)
