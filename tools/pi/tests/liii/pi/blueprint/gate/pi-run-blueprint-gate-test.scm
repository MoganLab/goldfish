(import (liii check)
  (liii string)
  (liii os)
  (liii path)
  (liii pi blueprint types)
  (liii pi blueprint gate)
) ;import

(check-set-mode! 'report-failed)

(define %true-cmd (if (os-windows?) "python3 -c pass" "true"))
(define %false-cmd (if (os-windows?) "python3 -c 1/0" "false"))
(define %sleep-cmd (if (os-windows?) "python3 -c \"import time; time.sleep(3)\"" "sleep 3"))
(define %pwd-cmd (if (os-windows?) "python3 -c \"import os; print(os.getcwd())\"" "pwd"))

(define (gate type desc command)
  (pi-blueprint-gate type desc command)
) ;define

;; pi-run-blueprint-gate
;; custom_command 通过。
(let ((res (pi-run-blueprint-gate (gate "custom_command" "ok" %true-cmd) 30)))
  (check (cdr (assoc 'passed res)) => #t)
  (check (cdr (assoc 'error res)) => #f)
  (check-true (number? (cdr (assoc 'duration-ms res))))
) ;let

;; custom_command 失败：返回错误摘要而非抛异常。
(let ((res (pi-run-blueprint-gate (gate "custom_command" "bad" %false-cmd) 30)))
  (check (cdr (assoc 'passed res)) => #f)
  (check-true (string? (cdr (assoc 'error res))))
) ;let

;; 超时必须触发并给出超时摘要。
(let ((res (pi-run-blueprint-gate (gate "custom_command" "slow" %sleep-cmd) 1)))
  (check (cdr (assoc 'passed res)) => #f)
  (check-true (string-contains? (cdr (assoc 'error res)) "timed out"))
) ;let

;; 门禁在当前工作目录执行。
(let ((res (pi-run-blueprint-gate (gate "custom_command" "pwd" %pwd-cmd) 30)))
  (check (cdr (assoc 'passed res)) => #t)
  (check-true (string-contains? (path-as-posix (path (cdr (assoc 'output res))))
                                (path-as-posix (path-cwd))))
) ;let

;; user_approval 未放行 → 未通过。
(let ((res (pi-run-blueprint-gate (gate "user_approval" "approve" #f) 30)))
  (check (cdr (assoc 'passed res)) => #f)
  (check-true (string-contains? (cdr (assoc 'error res)) "approval"))
) ;let

;; 缺少命令的 custom_command 直接失败。
(let ((res (pi-run-blueprint-gate (gate "custom_command" "no cmd" #f) 30)))
  (check (cdr (assoc 'passed res)) => #f)
) ;let

;; 不支持的类型给出可读错误。
(let ((res (pi-run-blueprint-gate (gate "tests_pass" "npm" #f) 30)))
  (check (cdr (assoc 'passed res)) => #f)
  (check-true (string-contains? (cdr (assoc 'error res)) "Unsupported"))
) ;let

;; :timeout 是必传参数并需为正数。
(check-catch 'value-error
  (pi-run-blueprint-gate (gate "custom_command" "ok" "true") #f)
) ;check-catch
(check-catch 'value-error
  (pi-run-blueprint-gate (gate "custom_command" "ok" "true") 0)
) ;check-catch

(check-report)
