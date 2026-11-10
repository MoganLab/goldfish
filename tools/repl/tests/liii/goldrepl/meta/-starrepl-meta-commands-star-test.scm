;; *repl-meta-commands*
;; 元命令表，每项形如 (命令名 说明 是否精确匹配)。
;;
;; 语法
;; ----
;; *repl-meta-commands*
;;
;; 参数
;; ----
;; 无
;; 该变量不接受参数。
;;
;; 返回值
;; ----
;; list
;; 元命令条目的列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl meta)
) ;import

(check-set-mode! 'report-failed)

;; 元命令表是一个列表
(check (list? *repl-meta-commands*) => #t)

;; 每个条目都是 (命令名 说明 精确匹配标志)
(check-true
  (every
    (lambda (cmd)
      (and (pair? cmd)
        (string? (car cmd))
        (string? (cadr cmd))
        (boolean? (caddr cmd))
      ) ;and
    ) ;lambda
    *repl-meta-commands*
  ) ;every
) ;check-true

;; 常用元命令都在表中
(check-true (if (assoc ",quit" *repl-meta-commands*) #t #f))
(check-true (if (assoc ",help" *repl-meta-commands*) #t #f))
(check-true (if (assoc ",import" *repl-meta-commands*) #t #f))
(check-true (if (assoc ",apropos" *repl-meta-commands*) #t #f))
(check-true (if (assoc ",describe" *repl-meta-commands*) #t #f))

(check-report)
