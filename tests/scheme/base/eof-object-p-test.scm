(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; eof-object?
;; 判断对象是否为文件结束对象。
;;
;; 语法
;; ----
;; (eof-object? obj)
;;
;; 参数
;; ----
;; obj : 任意类型
;; 待判断的对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果 obj 是 EOF 对象则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. EOF 对象表示输入流已到达末尾
;; 2. 同一个 Scheme 系统中所有 EOF 对象都是相同的
(check (eof-object? (eof-object)) => #t)
(check (eof-object? '()) => #f)
(check (eof-object? 0) => #f)
(check (eof-object? "") => #f)
(check (eof-object? #t) => #f)

(check-report)
