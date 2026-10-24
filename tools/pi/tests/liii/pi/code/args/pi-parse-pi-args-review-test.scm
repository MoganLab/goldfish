(import (liii check)
        (liii pi code args)
) ;import

(check-set-mode! 'report-failed)

;; pi-parse-args：code-review 相关开关
(let ((p (parse-pi-args '("--code-review"))))
  (check (pi-args-ref p 'code-review) => #t)
  (check (pi-args-ref p 'auto-review) => #f)
  (check (pi-args-ref p 'staged) => #f)
  (check (pi-args-ref p 'review-ref) => #f))

(let ((p (parse-pi-args '("--code-review" "--staged" "--ref=main" "src/a.ts" "src/b.py"))))
  (check (pi-args-ref p 'code-review) => #t)
  (check (pi-args-ref p 'staged) => #t)
  (check (pi-args-ref p 'review-ref) => "main")
  (check (pi-args-ref p 'messages) => '("src/a.ts" "src/b.py")))

;; 分离形式 --ref main
(let ((p (parse-pi-args '("--code-review" "--ref" "v1"))))
  (check (pi-args-ref p 'review-ref) => "v1")
  (check (pi-args-ref p 'messages) => '()))

(let ((p (parse-pi-args '("--auto-review" "实现功能"))))
  (check (pi-args-ref p 'auto-review) => #t)
  (check (pi-args-ref p 'messages) => '("实现功能")))

;; 未知选项仍进入 unknown-flags
(let ((p (parse-pi-args '("--refactor"))))
  (check (pi-args-ref p 'unknown-flags) => '("--refactor")))

;; pi-validate-review-flags
(check (pi-validate-review-flags (parse-pi-args '("--code-review"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--auto-review"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '())) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--code-review" "--staged"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--code-review" "--ref=main"))) => #t)

;; 冲突组合与依赖缺失均报可读错误
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--code-review" "--auto-review"))))
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--staged"))))
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--ref=main"))))

(check-report)
