;; check-table-node
;; 校验表格节点的对齐行、表头、表体与单元格结构。
;;
;; 语法
;; ----
;; (check-table-node node)
;;
;; 参数
;; ----
;; node : any
;; 待校验的 (table ...) 节点。
;;
;; 返回值
;; ----
;; unspecified
;; 校验通过时无返回值。
;;
;; 错误处理
;; ----
;; 字段数量、对齐枚举或行/单元格结构错误时抛出 md-stem-error。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown validate))

(check-set-mode! 'report-failed)

(check-true (procedure? check-table-node))

;; 合法表格
(check-true
  (check-table-node
    '(table (align default left)
       (thead (tr (th "Name") (th "Value")))
       (tbody (tr (td "A") (td "1")))
     )
  ) ;check-table-node
) ;check-true
(check-true
  (check-table-node
    '(table (align center right) (thead (tr (th "a"))) (tbody))
  ) ;check-table-node
) ;check-true

;; 字段数量错误
(check-catch 'md-stem-error (check-table-node '(table)))
(check-catch 'md-stem-error
  (check-table-node '(table (align default) (thead (tr (th "a")))))
) ;check-catch

;; 对齐行结构与枚举
(check-catch 'md-stem-error
  (check-table-node '(table (bogus default) (thead (tr (th "a"))) (tbody)))
) ;check-catch
(check-catch 'md-stem-error
  (check-table-node '(table (align bogus) (thead (tr (th "a"))) (tbody)))
) ;check-catch

;; 表头结构错误
(check-catch 'md-stem-error
  (check-table-node '(table (align default) (thead) (tbody)))
) ;check-catch
(check-catch 'md-stem-error
  (check-table-node '(table (align default) (tr (th "a")) (tbody)))
) ;check-catch

;; 表体行结构与单元格错误
(check-catch 'md-stem-error
  (check-table-node '(table (align default) (thead (tr (th "a"))) (tbody (p "x"))))
) ;check-catch
(check-catch 'md-stem-error
  (check-table-node '(table (align default) (thead (tr (td "a"))) (tbody)))
) ;check-catch
(check-catch 'md-stem-error
  (check-table-node '(table (align default) (thead (tr (th 42))) (tbody)))
) ;check-catch

(check-report)
