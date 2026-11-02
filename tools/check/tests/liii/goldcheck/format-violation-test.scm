;; format-violation
;; 将单条违规数据结构转换为对齐输出的描述字符串（通过 (liii goldcheck) 导出）。
;;
;; 语法
;; ----
;; (format-violation viol)
;;
;; 参数
;; ----
;; viol : list   违规项结构
;;
;; 返回值
;; ------
;; string
;; 格式化后的违规说明文本。

(import (scheme base) (liii check) (liii string) (liii goldcheck))

(check-set-mode! 'report-failed)

;; 1. 格式化 max-file-lines 违规
(let* ((viol (list 'max-file-lines "foo/bar.scm" 305 300))
       (msg (format-violation viol))
      ) ;
  (check-true (string-contains? msg "foo/bar.scm: max-file-lines: 305 (limit 300)")
  ) ;check-true
) ;let*

(check-report)
