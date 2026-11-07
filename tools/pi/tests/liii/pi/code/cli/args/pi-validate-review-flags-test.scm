;; pi-validate-review-flags
;; 校验 review 相关命令行参数的组合合法性。
;;
;; 语法
;; ----
;; (pi-validate-review-flags parsed)
;;
;; 参数
;; ----
;; parsed : alist
;; 解析后的参数字典。
;;
;; 返回值
;; ----
;; 不返回
;; 参数组合非法时抛出错误。

(import (liii check) (liii pi code cli args))

(check-set-mode! 'report-failed)

;; code-review 组合
(check (pi-validate-review-flags (parse-pi-args '("--code-review"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--auto-review"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '())) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--code-review" "--staged")))
  =>
  #t
) ;check
(check (pi-validate-review-flags (parse-pi-args '("--code-review" "--ref=main")))
  =>
  #t
) ;check

;; 冲突组合与依赖缺失均报可读错误
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--code-review" "--auto-review")))
) ;check-catch
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--staged")))
) ;check-catch
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--ref=main")))
) ;check-catch

;; simplify 组合
(check (pi-validate-review-flags (parse-pi-args '("--simplify"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--simplify" "--staged")))
  =>
  #t
) ;check
(check (pi-validate-review-flags (parse-pi-args '("--simplify" "--ref=main")))
  =>
  #t
) ;check
(check (pi-validate-review-flags (parse-pi-args '("--simplify" "--apply")))
  =>
  #t
) ;check
(check (pi-validate-review-flags (parse-pi-args '("--simplify"
                                                  "--apply"
                                                  "--staged")))
  =>
  #t
) ;check

(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--apply")))
) ;check-catch
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--simplify" "--code-review")))
) ;check-catch

(check-report)
