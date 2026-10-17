(import (liii check) (liii diff))

(check-set-mode! 'report-failed)

;; diff-ratio
;; 计算两个序列（字符串、列表或向量）的相似度得分（对标 Python difflib SequenceMatcher.ratio）。
;;
;; 语法
;; ----
;; (diff-ratio seq1 seq2)
;;
;; 参数
;; ----
;; seq1 : string | list | vector
;; 第一个待比较序列。
;; seq2 : string | list | vector
;; 第二个待比较序列。
;;
;; 返回值
;; -----
;; real
;; 返回 0.0 至 1.0 之间的实数，相同序列返回 1.0，完全无关序列返回 0.0。
;;
;; 错误处理
;; --------
;; type-error 当 seq1 或 seq2 不是字符串、列表或向量时。

;; 测试项1：相同序列相似度为 1.0
(check (diff-ratio "" "") => 1.0)
(check (diff-ratio "hello" "hello") => 1.0)
(check (diff-ratio '() '()) => 1.0)
(check (diff-ratio '(1 2 3) '(1 2 3)) => 1.0)
(check (diff-ratio #() #()) => 1.0)
(check (diff-ratio #(1 2 3) #(1 2 3)) => 1.0)

;; 测试项2：完全无关序列相似度为 0.0
(check (diff-ratio "abc" "xyz") => 0.0)
(check (diff-ratio '(1 2) '(3 4)) => 0.0)
(check (diff-ratio #(1 2) #(3 4)) => 0.0)

;; 测试项3：部分匹配相似度计算
;; "abcd" 与 "bcde"：公共子串 "bcd" 长度 3，ratio = 2 * 3 / (4 + 4) = 0.75
(check (diff-ratio "abcd" "bcde") => 0.75)

;; "abxcd" 与 "abcd"：匹配 "ab" (2) 与 "cd" (2)，ratio = 2 * 4 / (5 + 4) = 8/9
(let ((r (diff-ratio "abxcd" "abcd")))
  (check-true
    (< (abs (- r (/ 8.0 9.0))) 0.000001)
  ) ;check-true
) ;let

;; 列表与向量支持
(let ((r (diff-ratio '(1 2 3) '(1 2 4))))
  (check-true
    (< (abs (- r (/ 4.0 6.0))) 0.000001)
  ) ;check-true
) ;let

(let ((r (diff-ratio #(1 2 3) #(1 2 4))))
  (check-true
    (< (abs (- r (/ 4.0 6.0))) 0.000001)
  ) ;check-true
) ;let

;; 跨序列类型比对（列表与向量）
(check (diff-ratio '(1 2 3) #(1 2 3)) => 1.0)

;; 测试项4：非法入参类型防御
(check-catch 'type-error (diff-ratio 123 "abc"))
(check-catch 'type-error (diff-ratio "abc" 123))
(check-catch 'type-error (diff-ratio #t '()))
(check-catch 'type-error (diff-ratio '() 'symbol))

(check-report)
