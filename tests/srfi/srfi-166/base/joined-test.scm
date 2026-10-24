(import (liii check)
        (srfi srfi-166 base))

(check-set-mode! 'report-failed)

;; joined
;; 序列连接组合子，将列表各元素通过指定格式函数和分隔符连接输出。
;;
;; 语法
;; ----
;; (joined elt-f ls [sep])
;; (joined/prefix elt-f ls [sep])
;; (joined/suffix elt-f ls [sep])
;; (joined/last elt-f last-f ls [sep])
;; (joined/dot elt-f dot-f ls [sep])
;; (joined/range elt-f start [end] [sep])
;;
;; 示例
;; ----
;; (show #f (joined each '(1 2 3) " ")) => "1 2 3"
;; (show #f (joined/prefix each '("a" "b") ":")) => ":a:b"
;;
;; 另见
;; ----
;; (liii format base) 同名再导出本函数，日常使用更推荐导入该库：
;;   (import (liii format base))
;;   (show #f (joined each '(1 2 3) " "))

(check (show #f (joined each '(1 2 3) " ")) => "1 2 3")
(check (show #f (joined/prefix (lambda (x) (trimmed/right 3 x)) '("abcdef" "123456") ":"))
       => ":abc:123")
(check (show #f (joined/suffix (lambda (x) (trimmed/right 3 x)) '("abcdef" "123456") nl))
       => "abc\n123\n")
(check (show #f (joined/last each (lambda (x) (each "and " x)) '(lions tigers bears) ", "))
       => "lions, tigers, and bears")
(check (show #f (joined/dot each (lambda (x) (each "or " x)) '(lions tigers . bears) ", "))
       => "lions, tigers, or bears")
(check (show #f (joined/range each 1 4 ",")) => "1,2,3")

(check-report)
