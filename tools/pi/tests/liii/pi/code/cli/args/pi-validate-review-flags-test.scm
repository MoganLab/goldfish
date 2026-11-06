;; pi-validate-review-flags
;; 对解析结果依次运行扩展注册的校验器（如 --code-review/--simplify 组合
;; 合法性）；校验失败时由校验器抛错。
;;
;; 语法
;; ----
;; (pi-validate-review-flags parsed) -> #t
;;
;; 参数
;; ----
;; parsed : alist
;; parse-pi-args 解析结果。
;;
;; 返回值
;; ------
;; #t
;; 全部校验通过。

(import (scheme base)
  (liii check)
  (liii pi code cli args)
) ;import

(check-set-mode! 'report-failed)

;; 普通参数通过校验
(check (pi-validate-review-flags (parse-pi-args '("hello"))) => #t)

;; 非法组合抛错：--apply 依赖 --simplify
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--apply" "do something"))))

(check-report)
