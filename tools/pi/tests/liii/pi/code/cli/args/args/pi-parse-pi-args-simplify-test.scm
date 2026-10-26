(import (liii check)
        (liii pi code cli args)
) ;import

(check-set-mode! 'report-failed)

;; pi-parse-args：simplify 相关开关
(let ((p (parse-pi-args '("--simplify"))))
  (check (pi-args-ref p 'simplify) => #t)
  (check (pi-args-ref p 'apply) => #f)
  (check (pi-args-ref p 'staged) => #f)
  (check (pi-args-ref p 'review-ref) => #f))

(let ((p (parse-pi-args '("--simplify" "--apply" "--staged" "--ref=main"
                          "src/a.ts" "src/b.ts"))))
  (check (pi-args-ref p 'simplify) => #t)
  (check (pi-args-ref p 'apply) => #t)
  (check (pi-args-ref p 'staged) => #t)
  (check (pi-args-ref p 'review-ref) => "main")
  (check (pi-args-ref p 'messages) => '("src/a.ts" "src/b.ts")))

;; 分离形式 --ref main
(let ((p (parse-pi-args '("--simplify" "--ref" "v1"))))
  (check (pi-args-ref p 'review-ref) => "v1")
  (check (pi-args-ref p 'messages) => '()))

;; 未知选项仍进入 unknown-flags
(let ((p (parse-pi-args '("--simplify" "--simplifier"))))
  (check (pi-args-ref p 'unknown-flags) => '("--simplifier")))

;; pi-validate-review-flags：simplify 组合
(check (pi-validate-review-flags (parse-pi-args '("--simplify"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--simplify" "--staged"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--simplify" "--ref=main"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--simplify" "--apply"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--simplify" "--apply" "--staged"))) => #t)

;; --apply 单独出现报错
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--apply"))))

;; --simplify 与 --code-review 冲突
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--simplify" "--code-review"))))

;; 既有 code-review 校验不回归
(check (pi-validate-review-flags (parse-pi-args '("--code-review" "--staged"))) => #t)
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--staged"))))

(check-report)
