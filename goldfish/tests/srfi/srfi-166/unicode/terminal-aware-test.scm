;; terminal-aware
;; 终端宽度感知格式化组合子。
;;
;; 语法
;; ----
;; (terminal-aware fmt ...)
;;
;; 参数
;; ----
;; fmt : format?
;; 零个或多个格式化对象（字符串或其他格式化组合子）。
;;
;; 返回值
;; ----
;; format?
;; 内部宽度敏感操作按终端显示宽度处理的组合子。
;;
;; 说明
;; ----
;; 在组合子内部将 string-width 与截断函数替换为 Unicode
;; 感知版本：全角字符计 2 列、歧义字符按宽窄模式计 1 或 2 列、
;; ANSI 颜色转义序列计 0 列，使 padded、trimmed 等操作
;; 按真实显示宽度补齐与裁剪。

(import (liii check)
  (srfi srfi-166 base)
  (srfi srfi-166 color)
  (srfi srfi-166 unicode)
) ;import

(check-set-mode! 'report-failed)

(check
  (show #f (terminal-aware (with ((pad-char #\~)) (padded/both 10 "日本語"))))
  =>
  "~~日本語~~"
) ;check

(check (show #f (terminal-aware (trimmed 2 "日本語"))) => "語")
(check (show #f (terminal-aware (trimmed/right 2 "日本語"))) => "日")
(check (show #f (terminal-aware (trimmed/both 2 "日本語"))) => "本")

(check
  (show #f
    (terminal-aware (trimmed/right 2 (as-red "日本語" (on-cyan "!!!!"))))
  ) ;show
  =>
  "\x1b;[31m日\x1b;[46m\x1b;[49m\x1b;[39m"
) ;check

(check
  (show #f (terminal-aware "日本語" (fn (col) (each " col: " col))))
  =>
  "日本語 col: 6"
) ;check

(check
  (show #f (terminal-aware "日本語ΠΜΕ" (fn (col) (each " col: " col))))
  =>
  "日本語ΠΜΕ col: 9"
) ;check

(check
  (show #f
    (with ((ambiguous-is-wide? #t))
      (terminal-aware "日本語ΠΜΕ" (fn (col) (each " col: " col)))
    ) ;with
  ) ;show
  =>
  "日本語ΠΜΕ col: 12"
) ;check

(check-report)
