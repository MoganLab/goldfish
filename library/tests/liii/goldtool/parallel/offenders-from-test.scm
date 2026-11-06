;; offenders-from
;; 从结果列表中提取未通过的文件路径，状态为 #f 的结果视为未通过。
;;
;; 语法
;; ----
;; (offenders-from results)
;;
;; 参数
;; ----
;; results : list
;; 结果三元表列表，每项形如 (路径 状态 失败信息)。
;;
;; 返回值
;; ------
;; list
;; 未通过的文件路径列表，顺序与结果列表一致；无未通过项时返回空列表。

(import (scheme base) (liii check) (liii goldtool parallel))

(check-set-mode! 'report-failed)

;; 状态为 #f 的结果进入 offenders，其余被过滤
(check
  (offenders-from
    '(("a.scm" #f "syntax error")
      ("b.scm" ok "done")
      ("c.scm" #f "unexpected close paren")
    )
  ) ;offenders-from
  =>
  '("a.scm" "c.scm")
) ;check

;; 全部通过时返回空列表
(check (offenders-from '(("a.scm" ok "done"))) => '())

;; 空结果列表返回空列表
(check (offenders-from '()) => '())

(check-report)
