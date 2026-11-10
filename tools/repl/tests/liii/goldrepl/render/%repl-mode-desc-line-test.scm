;; %repl-mode-desc-line
;; 返回当前模式的说明行。
;;
;; 语法
;; ----
;; (%repl-mode-desc-line mode)
;;
;; 参数
;; ----
;; mode : string
;; REPL 模式名称，例如 "liii"、"r7rs"。
;;
;; 返回值
;; ----
;; string
;; 模式说明行。

(import (scheme base) (liii check) (liii string) (liii goldrepl render))

(check-set-mode! 'report-failed)

;; liii 模式
(let ((line (%repl-mode-desc-line "liii")))
  (check (string? line) => #t)
  (check-true (string-contains line "Mode:"))
  (check-true (string-contains line "liii"))
  ;; liii 模式会额外导入若干模块
  (check-true (string-contains line "additionally imports"))
) ;let

;; default 模式与 liii 模式一致
(let ((line (%repl-mode-desc-line "default")))
  (check-true (string-contains line "Mode:"))
  (check-true (string-contains line "additionally imports"))
) ;let

;; 其它模式只显示模式名
(let ((line (%repl-mode-desc-line "r7rs")))
  (check (string? line) => #t)
  (check-true (string-contains line "Mode:"))
  (check-true (string-contains line "r7rs"))
  (check (not (string-contains line "additionally imports")) => #t)
) ;let

(check-report)
