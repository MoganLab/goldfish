;; pi-last-compaction-summary
;; 读取会话条目中最近一次压缩的摘要。
;;
;; 语法
;; ----
;; (pi-last-compaction-summary entries)
;;
;; 参数
;; ----
;; entries : list
;; 会话条目列表。
;;
;; 返回值
;; ----
;; (or string #f)
;; 最近一次压缩的摘要文本，不存在时返回 #f。

(import (liii check) (liii pi code core compaction session))

(check-set-mode! 'report-failed)

;; 1. 没有压缩记录时返回 #f
(check (pi-last-compaction-summary '()) => #f)
(check (pi-last-compaction-summary '((type . "message") (id . "m1"))) => #f)

;; 2. 读取最近一次压缩记录的摘要
(let ((entries (list (list (cons "type" "compaction") (cons "summary" "first"))
                 (list (cons "type" "message") (cons "id" "m1"))
                 (list (cons "type" "compaction") (cons "summary" "latest"))
               ) ;list
      ) ;entries
     ) ;
  (check (pi-last-compaction-summary entries) => "latest")
) ;let

(check-report)
