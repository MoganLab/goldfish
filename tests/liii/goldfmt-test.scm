(import (liii check) (liii goldfmt) (srfi srfi-13))

(check-set-mode! 'report-failed)

;; 1. format-string 基础测试
(check (format-string "(define (add a b) (+ a b))")
  =>
  "(define (add a b)\n  (+ a b)\n) ;define\n"
) ;check

;; 2. format-scheme-string 别名测试
(check (format-scheme-string "(define (add a b) (+ a b))")
  =>
  "(define (add a b)\n  (+ a b)\n) ;define\n"
) ;check

;; 3. 复杂 Scheme 表达式格式化
(check (format-string "(let ((x 1) (y 2)) (+ x y))")
  =>
  "(let ((x 1) (y 2))\n  (+ x y)\n) ;let\n"
) ;check

;; 4. format-stem-string 测试：确保 quote / unquote 保持原样不被糖化

(define stem-sample
  (format-stem-string "(document (chapter* (unquote title)) (quote (a b)))")
) ;define
(check-true (string? stem-sample))
(check-true (string-contains stem-sample "(unquote title)"))
(check-true (string-contains stem-sample "(quote (a b))"))
(check-false (string-contains stem-sample ",title"))
(check-false (string-contains stem-sample "'(a b)"))

;; 5. 重新导出的 API 测试
(check-true (can-inline? (vector-ref (scan-string "(+ 1 2)") 0)))
(check-true (vector? (scan-string "(+ 1 2)")))
(check-true (string? (format-datum '(define x 1))))

(check-report)
