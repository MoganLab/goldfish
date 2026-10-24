(import (scheme base)
  (liii check)
  (liii string)
  (liii goldcheck)
) ;import

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
  (check-true (string-contains? (list-ref lines 5) "... and 2 more no-crlf violation(s)"))
) ;let*

;; 4. 两条规则各超上限：每规则展示 5 条并各自汇总
(let* ((vs (vector-append
             (vector (list 'no-crlf "a.scm" 1)
                   (list 'no-crlf "b.scm" 1)
                   (list 'no-crlf "c.scm" 2)
                   (list 'no-crlf "d.scm" 3)
                   (list 'no-crlf "e.scm" 4)
                   (list 'no-crlf "f.scm" 5)
              ) ;vector
             (vector (list 'max-file-lines "x.scm" 405 400)
                   (list 'max-file-lines "y.scm" 401 400)
              ) ;vector
           ) ;vector-append
       ) ;vs
       (lines (violation-report-lines vs 5))
      ) ;
  (check (length lines) => 8)
  (check-true (string-contains? (list-ref lines 5) "... and 1 more no-crlf violation(s)"))
  (check-true (string-contains? (list-ref lines 6) "x.scm: max-file-lines: 405 (limit 400)"))
) ;let*

;; 5. cap 为 #f 时不封顶全部展示
(let* ((vs (vector (list 'no-crlf "a.scm" 1)
                 (list 'no-crlf "b.scm" 1)
                 (list 'no-crlf "c.scm" 2)
                 (list 'no-crlf "d.scm" 3)
                 (list 'no-crlf "e.scm" 4)
                 (list 'no-crlf "f.scm" 5)
                 (list 'no-crlf "g.scm" 6)
              ) ;vector
       ) ;vs
       (lines (violation-report-lines vs #f))
      ) ;
  (check (length lines) => 7)
) ;let*

;; violation-display-hint
;; 生成封顶截断时的提示行；未截断或未封顶时返回 #f。

;; 6. 截断时生成提示：包含展示数与总数
(let ((hint (violation-display-hint
              (vector (list 'no-crlf "a.scm" 1)
                (list 'no-crlf "b.scm" 1)
                (list 'no-crlf "c.scm" 2)
                (list 'no-crlf "d.scm" 3)
                (list 'no-crlf "e.scm" 4)
                (list 'no-crlf "f.scm" 5)
                (list 'no-crlf "g.scm" 6)
              ) ;vector
              5
            ) ;violation-display-hint
       ) ;hint
      ) ;
  (check-true (string-contains? hint "Only 5 of 7 violation(s) displayed"))
  (check-true (string-contains? hint "gf check --only=<rule>"))
) ;let*

;; 7. 未截断时返回 #f
(check (violation-display-hint
         (vector (list 'no-crlf "a.scm" 1) (list 'no-crlf "b.scm" 1))
         5
       ) ;violation-display-hint
  => #f
) ;check

;; 8. cap 为 #f（不封顶）时返回 #f
(check (violation-display-hint
         (vector (list 'no-crlf "a.scm" 1) (list 'no-crlf "b.scm" 1))
         #f
       ) ;violation-display-hint
  => #f
) ;check

(check-report)
