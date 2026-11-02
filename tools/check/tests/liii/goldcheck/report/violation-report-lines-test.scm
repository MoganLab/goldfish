;; violation-report-lines
;; 将违规向量转为展示行：同一规则最多展示 cap 条，超出部分汇总为一行提示。
;;
;; 语法
;; ----
;; (violation-report-lines violations cap)
;;
;; 参数
;; ----
;; violations : vector of violation
;; 按规则连续分组的违规向量（run-rule-check 拼接后即为该形态）。
;; cap : integer
;; 每条规则最多展示的违规条数。
;;
;; 返回值
;; ------
;; list of string
;; 待展示的行列表：每规则至多 cap 条违规行 + 超限汇总行。

(import (scheme base) (liii check) (liii string) (liii goldcheck report))

(check-set-mode! 'report-failed)

;; 1. 空向量：无输出行
(check (violation-report-lines '#() 5) => '())

;; 2. 单规则 3 条（未超上限）：全部展示，无汇总行
(let* ((vs (vector (list 'no-crlf "a.scm" 1)
             (list 'no-crlf "b.scm" 1)
             (list 'no-crlf "c.scm" 2)
           ) ;vector
       ) ;vs
       (lines (violation-report-lines vs 5))
      ) ;
  (check (length lines) => 3)
) ;let*

;; 3. 单规则 7 条、上限 5：展示 5 条 + 1 条汇总行
(let* ((vs (vector (list 'no-crlf "a.scm" 1)
             (list 'no-crlf "b.scm" 1)
             (list 'no-crlf "c.scm" 2)
             (list 'no-crlf "d.scm" 3)
             (list 'no-crlf "e.scm" 4)
             (list 'no-crlf "f.scm" 5)
             (list 'no-crlf "g.scm" 6)
           ) ;vector
       ) ;vs
       (lines (violation-report-lines vs 5))
      ) ;
  (check (length lines) => 6)
  (check-true (string-contains? (list-ref lines 5) "... and 2 more no-crlf violation(s)")
  ) ;check-true
) ;let*

;; 4. 多规则混排（已按规则分组）：各自遵守上限
(let* ((vs (vector (list 'no-crlf "a.scm" 1)
             (list 'no-crlf "b.scm" 1)
             (list 'no-crlf "c.scm" 2)
             (list 'max-file-lines "x.scm" 10 5)
           ) ;vector
       ) ;vs
       (lines (violation-report-lines vs 2))
      ) ;
  (check (length lines) => 4)
  (check-true (string-contains? (list-ref lines 2) "... and 1 more no-crlf violation(s)")
  ) ;check-true
) ;let*

;; 5. cap 为 #f 或未封顶时：展示全部
(let* ((vs (vector (list 'no-crlf "a.scm" 1)
             (list 'no-crlf "b.scm" 1)
             (list 'no-crlf "c.scm" 2)
           ) ;vector
       ) ;vs
       (lines (violation-report-lines vs #f))
      ) ;
  (check (length lines) => 3)
) ;let*

(check-report)
