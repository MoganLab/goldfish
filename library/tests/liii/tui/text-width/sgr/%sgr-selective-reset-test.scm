;; %sgr-selective-reset
;; 根据一串生效中的 SGR 序列，生成只复位其实际生效样式的选择性复位串，
;; 避免用 \x1b[0m 全重置清掉行外设置的背景等样式（issue #1484）。
;;
;; 语法
;; ----
;; (%sgr-selective-reset active-ansi)
;;
;; 参数
;; ----
;; active-ansi : string?，行内累积的转义序列串（可含非 SGR 序列，将被忽略）
;;
;; 返回值
;; ----
;; string?，一条参数升序合并的选择性复位 SGR（如 "\x1b[39;49m"）；
;; 无生效样式时返回 ""。

(import (liii check) (liii tui text-width sgr))

(check-set-mode! 'report-failed)

;; 空串与纯文本无样式可复位
(check (%sgr-selective-reset "") => "")
(check (%sgr-selective-reset "plain text") => "")

;; 前景 / 背景 / 粗体分别映射到选择性复位参数
(check (%sgr-selective-reset "\x1b;[31m") => "\x1b;[39m")
(check (%sgr-selective-reset "\x1b;[48;2;52;56;58m") => "\x1b;[49m")
(check (%sgr-selective-reset "\x1b;[1m") => "\x1b;[22m")
(check (%sgr-selective-reset "\x1b;[4m") => "\x1b;[24m")

;; 多样式合并为一条 SGR，参数升序去重
(check (%sgr-selective-reset "\x1b;[48;2;52;56;58m\x1b;[38;2;157;165;169m")
  =>
  "\x1b;[39;49m"
) ;check
(check (%sgr-selective-reset "\x1b;[1;31m") => "\x1b;[22;39m")
(check (%sgr-selective-reset "\x1b;[1m\x1b;[2m") => "\x1b;[22m")

;; 已选择性复位的样式不再输出；全重置清空全部状态
(check (%sgr-selective-reset "\x1b;[31m\x1b;[39m") => "")
(check (%sgr-selective-reset "\x1b;[1;31m\x1b;[22m") => "\x1b;[39m")
(check (%sgr-selective-reset "\x1b;[1;31m\x1b;[0m") => "")
(check (%sgr-selective-reset "\x1b;[31m\x1b;[m") => "")

;; 亮色前景 90-97 / 亮色背景 100-107 也按前景/背景跟踪
(check (%sgr-selective-reset "\x1b;[91m") => "\x1b;[39m")
(check (%sgr-selective-reset "\x1b;[101m") => "\x1b;[49m")

;; 非 SGR 的 CSI / OSC 序列忽略，不影响状态
(check (%sgr-selective-reset "\x1b;[2K\x1b;[31m") => "\x1b;[39m")
(check (%sgr-selective-reset "\x1b;]8;;http://x\x07;") => "")

;; 未收录的参数不跟踪、不复位
(check (%sgr-selective-reset "\x1b;[53m") => "")

(check-report)
