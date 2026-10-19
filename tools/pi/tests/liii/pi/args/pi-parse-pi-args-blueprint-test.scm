(import (liii check)
        (liii string)
        (liii pi args)
) ;import

(check-set-mode! 'report-failed)

;; pi-parse-pi-args：blueprint 五个开关与 --approve / --blueprint-file。
(check (pi-args-ref (parse-pi-args '("--blueprint" "为用户服务加入登录")) 'blueprint)
  =>
  #t
) ;check
(check (pi-args-ref (parse-pi-args '("--blueprint" "目标")) 'messages)
  =>
  '("目标")
) ;check
(check (pi-args-ref (parse-pi-args '("--plan-status")) 'plan-status) => #t)
(check (pi-args-ref (parse-pi-args '("--plan-next")) 'plan-next) => #t)
(check (pi-args-ref (parse-pi-args '("--plan-verify")) 'plan-verify) => #t)
(check (pi-args-ref (parse-pi-args '("--plan-abandon")) 'plan-abandon) => #t)
(check (pi-args-ref (parse-pi-args '("--plan-verify" "--approve")) 'approve) => #t)

(check (pi-args-ref (parse-pi-args '("--blueprint-file" "x.json")) 'blueprint-file)
  =>
  "x.json"
) ;check
(check (pi-args-ref (parse-pi-args '("--blueprint-file=x.json")) 'blueprint-file)
  =>
  "x.json"
) ;check

;; 互斥：五开关只能选一个。
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--blueprint" "x" "--plan-status")))
) ;check-catch
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--plan-next" "--plan-verify")))
) ;check-catch
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--plan-verify" "--plan-abandon")))
) ;check-catch

;; 与既有工作流开关互斥。
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--blueprint" "x" "--code-review")))
) ;check-catch
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--blueprint" "x" "--simplify")))
) ;check-catch
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--plan-next" "--red-green" "x")))
) ;check-catch

;; --approve 只能和 --plan-verify 同用。
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--approve")))
) ;check-catch
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--plan-status" "--approve")))
) ;check-catch
(check (pi-validate-review-flags (parse-pi-args '("--plan-verify" "--approve")))
  =>
  #t
) ;check

;; 单个开关合法。
(check (pi-validate-review-flags (parse-pi-args '("--blueprint" "x"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--plan-status"))) => #t)

;; 未使用 blueprint 开关时默认值全为 #f。
(check (pi-args-ref (parse-pi-args '("hello")) 'blueprint) => #f)
(check (pi-args-ref (parse-pi-args '("hello")) 'plan-status) => #f)
(check (pi-args-ref (parse-pi-args '("hello")) 'blueprint-file) => #f)

(check-report)
