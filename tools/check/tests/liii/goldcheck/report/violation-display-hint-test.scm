;; violation-display-hint
;; 生成封顶截断时的提示行；未截断或未封顶（cap 为 #f）时返回 #f。
;;
;; 语法
;; ----
;; (violation-display-hint violations cap)
;;
;; 参数
;; ----
;; violations : vector   违规向量
;; cap : integer 或 #f   违规显示上限封顶数值
;;
;; 返回值
;; ------
;; string 或 #f
;; 提示文本，无截断或未启用封顶时返回 #f。

(import (scheme base) (liii check) (liii string) (liii goldcheck report))

(check-set-mode! 'report-failed)

;; 1. cap 为 #f 时返回 #f
(check (violation-display-hint '#() #f) => #f)

;; 2. 数量未超限返回 #f
(check (violation-display-hint '#((max-file-lines "a.scm" 10 5)) 5) => #f)

;; 3. 数量超限返回提示字符串
(let* ((vs (vector '(no-crlf "a.scm" 1) '(no-crlf "b.scm" 1) '(no-crlf "c.scm"
                                                                1)))
       (hint (violation-display-hint vs 2))
      ) ;
  (check-true (string? hint))
  (check-true (string-contains? hint "run 'gf check --only=<rule>'"))
) ;let*

(check-report)
