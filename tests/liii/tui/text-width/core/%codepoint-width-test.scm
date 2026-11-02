;; %codepoint-width
;; 计算单个 Unicode 码点在终端中的显示列宽。
;;
;; 语法
;; ----
;; (%codepoint-width cp)
;;
;; 参数
;; ----
;; cp : integer?，Unicode 码点整数值
;;
;; 返回值
;; ----
;; integer，返回 0、1 或 2，表示占用的终端视觉列宽。

(import (liii check) (liii tui text-width core))

(check-set-mode! 'report-failed)

;; 控制字符与零宽字符（宽 0）
(check (%codepoint-width 0) => 0)
(check (%codepoint-width 27) => 0)
(check (%codepoint-width 31) => 0)
(check (%codepoint-width 127) => 0)
(check (%codepoint-width 8203) => 0)
(check (%codepoint-width 65279) => 0)

;; ASCII 可见字符（宽 1）
(check (%codepoint-width 32) => 1)
(check (%codepoint-width 65) => 1)
(check (%codepoint-width 126) => 1)

;; CJK 表意字符与全角字符（宽 2）
(check (%codepoint-width 19968) => 2)
(check (%codepoint-width 20320) => 2)
(check (%codepoint-width 65281) => 2)

;; 常见 Emoji 字符（宽 2）
(check (%codepoint-width 128522) => 2)
(check (%codepoint-width 128640) => 2)

(check-report)
