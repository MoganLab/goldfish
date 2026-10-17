(import (liii check)
        (liii format base)
        (liii format color)
        (liii format unicode))

(check-set-mode! 'report-failed)

;; terminal-aware
;; 终端宽度感知组合子。使内部格式化操作（如 padded、trimmed）感知 Unicode 宽字符（如中日韩文字）和 ANSI 颜色转义序列的实际显示宽度。
;;
;; 语法
;; ----
;; (terminal-aware fmt ...)
;;
;; 示例
;; ----
;; (show #f (terminal-aware (padded/both 10 "日本語"))) => "  日本語  "

(check (show #f (terminal-aware (with ((pad-char #\~)) (padded/both 10 "日本語"))))
       => "~~日本語~~")

(check (show #f (terminal-aware (trimmed 2 "日本語"))) => "語")
(check (show #f (terminal-aware (trimmed/right 2 "日本語"))) => "日")
(check (show #f (terminal-aware (trimmed/both 2 "日本語"))) => "本")

(check (show #f (terminal-aware
                 (trimmed/right 2 (as-red "日本語" (on-cyan "!!!!")))))
       => "\x1b[31m日\x1b[46m\x1b[49m\x1b[39m")

(check (show #f (terminal-aware "日本語" (fn (col) (each " col: " col))))
       => "日本語 col: 6")

(check (show #f (terminal-aware "日本語ΠΜΕ" (fn (col) (each " col: " col))))
       => "日本語ΠΜΕ col: 9")

(check (show #f (with ((ambiguous-is-wide? #t))
                  (terminal-aware "日本語ΠΜΕ"
                                  (fn (col) (each " col: " col)))))
       => "日本語ΠΜΕ col: 12")

(check-report)
