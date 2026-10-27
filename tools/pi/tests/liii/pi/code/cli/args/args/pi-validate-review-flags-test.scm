(import (liii check)
  (liii pi code cli args)
) ;import

(check-set-mode! 'report-failed)

;; code-review 组合
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

;; simplify 组合
(check (pi-validate-review-flags (parse-pi-args '("--simplify"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--simplify" "--staged"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--simplify" "--ref=main"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--simplify" "--apply"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--simplify" "--apply" "--staged"))) => #t)

(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--apply"))))
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--simplify" "--code-review"))))

(check-report)
