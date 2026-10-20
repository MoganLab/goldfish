(import (liii check)
        (liii string)
        (liii pi args)
) ;import

(check-set-mode! 'report-failed)

;; pi-parse-pi-args：--red-green 解析
(check (pi-args-ref (parse-pi-args '("--red-green" "实现登录")) 'red-green) => #t)
(check (pi-args-ref (parse-pi-args '("--red-green" "实现登录")) 'messages)
  =>
  '("实现登录")
) ;check
(check (pi-args-ref (parse-pi-args '("实现登录")) 'red-green) => #f)

;; --red-green 与审查类开关互斥。
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--red-green" "--code-review" "x")))
) ;check-catch
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--red-green" "--simplify" "x")))
) ;check-catch
;; --red-green 不接受 review 专属的参数。
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--red-green" "--apply" "x")))
) ;check-catch
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--red-green" "--staged" "x")))
) ;check-catch
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--red-green" "--ref" "HEAD" "x")))
) ;check-catch

;; 单独使用 --red-green 合法。
(check (pi-validate-review-flags (parse-pi-args '("--red-green" "实现登录"))) => #t)

;; 原有开关不受影响。
(check (pi-args-ref (parse-pi-args '("--simplify")) 'simplify) => #t)
(check (pi-args-ref (parse-pi-args '("--code-review")) 'code-review) => #t)

;; 未知选项仍进入 unknown-flags。
(check (pi-args-ref (parse-pi-args '("--red-green" "--not-a-real-flag" "x"))
         'unknown-flags
       ) ;pi-args-ref
  =>
  '("--not-a-real-flag")
) ;check

(check-report)
