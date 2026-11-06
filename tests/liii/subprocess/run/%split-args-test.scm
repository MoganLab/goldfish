;; %split-args
;; 将组合命令的参数列表拆分为命令列表和尾部关键字选项。
;;
;; 语法
;; ----
;; (%split-args args)
;;
;; 参数
;; ----
;; args : list
;; 命令及可选的尾部关键字键值对组成的列表。
;;
;; 返回值
;; ----
;; 两个值：commands 和 options。
;; commands 为命令列表，options 为保持原顺序的关键字选项列表。

(import (liii check) (liii subprocess run))

(check-set-mode! 'report-failed)

(let-values (((commands options) (%split-args '("true"
                                                "false"
                                                :cwd     "/tmp"
                                                :timeout 2))))
  (check commands => '("true" "false"))
  (check options => '(:cwd "/tmp" :timeout 2))
) ;let-values

(let-values (((commands options) (%split-args '("true" (false)))))
  (check commands => '("true" (false)))
  (check options => '())
) ;let-values

(let-values (((commands options) (%split-args '(:cwd "/tmp" :env ()))))
  (check commands => '())
  (check options => '(:cwd "/tmp" :env ()))
) ;let-values

(let-values (((commands options) (%split-args '())))
  (check commands => '())
  (check options => '())
) ;let-values

(check-report)
