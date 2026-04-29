(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-tabulate
;; 通过将过程应用于索引来构造字符串。
;;
;; 语法
;; ----
;; (string-tabulate proc len)
;;
;; 参数
;; ----
;; proc : procedure
;; 接收整数索引（从0开始）并返回字符的过程
;;
;; len : integer
;; 构造的字符串长度
;;
;; 返回值
;; ------
;; string?
;; 构造的新字符串
;;
;; 说明
;; ----
;; 1. (proc i) 对每个 i 从 0 到 len-1 调用
;; 2. 与 (liii string) 的区别：无直接对应函数，(liii string-cursor) 独有
;; 3. 性能：O(n)，n 为 len

(check (string-tabulate (lambda (i) (integer->char (+ i 65))) 3) => "ABC")
(check (string-tabulate (lambda (i) #\a) 0) => "")
(check (string-tabulate (lambda (i) (string-ref/cursor "中文" (string-index->cursor "中文" i)))
         2
       ) ;string-tabulate
  =>
  "中文"
) ;check

(check-report)
