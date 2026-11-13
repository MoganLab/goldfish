;; make-session-search-cache
;; 创建用于会话选择器搜索的缓存表。
;;
;; 语法
;; ----
;; (make-session-search-cache)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; hash-table
;; 返回以 eq 比较会话对象的哈希表。

(import (scheme base)
  (liii check)
  (liii hash-table)
  (liii pi code modes interactive session-selector-search)
) ;import

(check-set-mode! 'report-failed)

(let ((cache (make-session-search-cache)))
  (check-true (hash-table? cache))
  (check-true (hash-table-empty? cache))
) ;let

(check-report)
