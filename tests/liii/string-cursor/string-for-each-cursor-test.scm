(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-for-each-cursor
;; 对字符串中的每个字符位置（cursor）应用给定的过程。
;;
;; 语法
;; ----
;; (string-for-each-cursor proc s)
;;
;; 参数
;; ----
;; proc : procedure
;; 一元过程，接收一个 string-cursor 参数
;;
;; s : string
;; 要遍历的字符串
;;
;; 返回值
;; ------
;; undefined
;; 过程执行完毕，无返回值
;;
;; 说明
;; ----
;; 1. 遍历顺序为从左到右
;; 2. 传递给 proc 的是 cursor 而非字符本身
;; 3. 需要使用 string-ref/cursor 等函数从 cursor 获取字符
;; 4. 性能：O(n)，n 为字符串字符数

;; 基本测试 - 收集所有 cursor 并通过 string-ref/cursor 读取字符
(let ((result '()))
  (string-for-each-cursor
    (lambda (cur) (set! result (cons cur result)))
    "abc")
  (check (map (lambda (c) (string-ref/cursor "abc" c)) (reverse result)) => '(#\a #\b #\c)))

(check-report)
