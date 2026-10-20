(import (scheme base)
  (scheme process-context)
  (liii check)
  (liii tui text-width)
  (demo tui snake style)
) ;import

(check-set-mode! 'report-failed)

;; 1. 样式默认关闭，调用 style-enable! 后开启
(check (style-enabled?) => #f)
(style-enable!)
(check (style-enabled?) => #t)

(let ((head-str (style-head "@")))
  (check (string? head-str) => #t)
  (check (> (string-length head-str) 1) => #t)
  (check (display-width head-str) => 1)
  (check (string=? (strip-ansi head-str) "@") => #t))

(let ((body-str (style-body "o")))
  (check (string? body-str) => #t)
  (check (> (string-length body-str) 1) => #t)
  (check (display-width body-str) => 1)
  (check (string=? (strip-ansi body-str) "o") => #t))

(let ((food-str (style-food "*")))
  (check (string? food-str) => #t)
  (check (> (string-length food-str) 1) => #t)
  (check (display-width food-str) => 1)
  (check (string=? (strip-ansi food-str) "*") => #t))

(let ((title-str (style-title " STATS ")))
  (check (display-width title-str) => 7)
  (check (string=? (strip-ansi title-str) " STATS ") => #t))

(let ((accent-str (style-accent "100")))
  (check (display-width accent-str) => 3)
  (check (string=? (strip-ansi accent-str) "100") => #t))

(let ((dim-str (style-dim "Arrows: Move")))
  (check (display-width dim-str) => 12)
  (check (string=? (strip-ansi dim-str) "Arrows: Move") => #t))

(let ((hb-str (style-heartbeat "+")))
  (check (display-width hb-str) => 1)
  (check (string=? (strip-ansi hb-str) "+") => #t))

(let ((go-str (style-game-over "GAME OVER")))
  (check (display-width go-str) => 9)
  (check (string=? (strip-ansi go-str) "GAME OVER") => #t))

;; 2. 动态开关与 with-color-mode 测试
(with-color-mode #f
  (lambda ()
    (check (style-enabled?) => #f)
    (check (style-head "@") => "@")
    (check (style-body "o") => "o")
    (check (style-food "*") => "*")
    (check (style-title " STATS ") => " STATS ")
    (check (style-accent "42") => "42")
    (check (style-dim "Help") => "Help")
    (check (style-heartbeat "|") => "|")
    (check (style-game-over "RECORD") => "RECORD")))

;; with-color-mode 退出后自动恢复原状态
(check (style-enabled?) => #t)
(check (string=? (strip-ansi (style-head "@")) "@") => #t)

;; style-disable! 与 style-enable!
(style-disable!)
(check (style-enabled?) => #f)
(check (style-head "@") => "@")

(style-enable!)
(check (style-enabled?) => #t)
(check (not (string=? (style-head "@") "@")) => #t)

;; 3. 类型错误检查
(check-catch 'type-error (style-head 123))
(check-catch 'type-error (style-body #f))
(check-catch 'type-error (style-food 'food))
(check-catch 'type-error (style-title '(1 2)))

(check-report)
