;; string-index-of
;; 从指定起始位置检索子串在母串中首次出现的索引。
;;
;; 语法
;; ----
;; (string-index-of str target start)
;;
;; 参数
;; ----
;; str : string      母串
;; target : string   待检索的目标子串
;; start : integer   起始检索下标（基于 0）
;;
;; 返回值
;; ------
;; integer 或 #f
;; 命中时返回下标位置，未命中返回 #f。

(import (scheme base) (liii check) (liii goldcheck path))

(check-set-mode! 'report-failed)

;; 1. 从开头成功检索
(check (string-index-of "hello world" "world" 0) => 6)

;; 2. 指定从不同下标检索
(check (string-index-of "abcabc" "abc" 0) => 0)
(check (string-index-of "abcabc" "abc" 1) => 3)

;; 3. 未找到目标子串返回 #f
(check (string-index-of "hello world" "foo" 0) => #f)

;; 4. 超出母串长度检索返回 #f
(check (string-index-of "abc" "abc" 4) => #f)

(check-report)
