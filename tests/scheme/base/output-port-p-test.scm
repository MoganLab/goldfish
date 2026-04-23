(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; output-port?
;; 判断对象是否为输出端口。
;;
;; 语法
;; ----
;; (output-port? obj)
;;
;; 参数
;; ----
;; obj : 任意类型
;; 待判断的对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果 obj 是输出端口则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 包括文件输出端口和字符串输出端口
;; 2. 关闭的端口仍然返回 #t
(check (output-port? (current-output-port)) => #t)
(check (output-port? (open-output-string)) => #t)
(check (output-port? (current-input-port)) => #f)
(check (output-port? 1) => #f)
(check (output-port? "str") => #f)

(check-report)
