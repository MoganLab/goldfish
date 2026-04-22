(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; input-port?
;; 判断对象是否为输入端口。
;;
;; 语法
;; ----
;; (input-port? obj)
;;
;; 参数
;; ----
;; obj : 任意类型
;; 待判断的对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果 obj 是输入端口则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 包括文件输入端口和字符串输入端口
;; 2. 关闭的端口仍然返回 #t
(check (input-port? (current-input-port)) => #t)
(check (input-port? (open-input-string "test")) => #t)
(check (input-port? (current-output-port)) => #f)
(check (input-port? 1) => #f)
(check (input-port? "str") => #f)

(check-report)
