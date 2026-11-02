;; assoc
;; 兼容访问事件与消息 record 或标准 alist 的键值。
;;
;; 语法
;; ----
;; (assoc key obj)
;;
;; 参数
;; ----
;; key : any?
;; 待查找的键（支持 symbol 或 string）。
;;
;; obj : any?
;; 待查找的关联列表或 record 对象。
;;
;; 返回值
;; ----
;; pair? 或 #f
;; 找到返回 (key . value) 点对，否则返回 #f。

(import (liii check) (liii pi ai event))

(check-set-mode! 'report-failed)

(let ((data '(("a" . 1) (b . 2))))
  (check (cdr (assoc "a" data)) => 1)
  (check (cdr (assoc 'b data)) => 2)
  (check (assoc "c" data) => #f)
) ;let

(check-report)
