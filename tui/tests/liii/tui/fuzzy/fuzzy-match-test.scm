;; fuzzy-match
;; 评估查询字符串与目标文本的模糊匹配情况并计算匹配分数。
;;
;; 语法
;; ----
;; (fuzzy-match query text)
;;
;; 参数
;; ----
;; query : string
;; 搜索查询字符串。
;;
;; text : string
;; 目标文本字符串。
;;
;; 返回值
;; ----
;; alist
;; 包含匹配状态与分数的关联列表：((matches . boolean) (score . number))。

(import (scheme base) (liii check) (liii tui fuzzy))

(check-set-mode! 'report-failed)

;; 1. 完全匹配与匹配标识
(let ((res (fuzzy-match "abc" "abc")))
  (check (cdr (assoc 'matches res)) => #t)
  (check (< (cdr (assoc 'score res)) 0) => #t)
) ;let

;; 2. 不匹配情况
(let ((res (fuzzy-match "xyz" "abc")))
  (check (cdr (assoc 'matches res)) => #f)
  (check (cdr (assoc 'score res)) => 0)
) ;let

;; 3. 子串及大小写不敏感匹配
(let ((res (fuzzy-match "bar" "FooBarBaz")))
  (check (cdr (assoc 'matches res)) => #t)
) ;let

;; 4. 字母数字相邻交换匹配
(let ((res (fuzzy-match "5gpt" "gpt5")))
  (check (cdr (assoc 'matches res)) => #t)
) ;let

;; 5. 非法 UTF-8 字节序列容错（不抛错，仍可匹配合法部分）
(let ((bad-text (string #\d #\e (integer->char #x80) #\b #\u #\g))
      (bad-query (string #\d (integer->char #xe4) #\b))
     ) ;
  (check (cdr (assoc 'matches (fuzzy-match "debug" bad-text))) => #t)
  (check (cdr (assoc 'matches (fuzzy-match bad-query "debug"))) => #f)
) ;let

(check-report)
