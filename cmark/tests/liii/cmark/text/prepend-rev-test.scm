;; prepend-rev
;; 返回 (append (reverse l) acc)，尾共享 acc，避免深拷贝。
;;
;; 语法
;; ----
;; (prepend-rev l acc)
;;
;; 参数
;; ----
;; l : list
;; 待反转并前置的列表。
;; acc : list
;; 累积列表。
;;
;; 返回值
;; ----
;; list
;; 反转后的 l 拼接在 acc 前的新列表。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check (prepend-rev '(1 2) '()) => '(2 1))
(check (prepend-rev '(3) '(1 2)) => '(3 1 2))
(let ((acc '(a b)) (l '(1)))
  (check (equal? (prepend-rev l acc) '(1 a b)) => #t)
) ;let

(check-report)
