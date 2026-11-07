;; pi-user-dict-ref
;; 从用户配置字典中读取指定键的取值。
;;
;; 语法
;; ----
;; (pi-user-dict-ref obj key default-val)
;;
;; 参数
;; ----
;; obj : alist
;; 配置字典。
;;
;; key : (or string symbol)
;; 字段名，兼容字符串与符号键。
;;
;; default-val : any
;; 字段缺失时的默认值。
;;
;; 返回值
;; ----
;; any
;; 字段取值，缺失时返回默认值。

(import (liii check) (liii pi code config user-fs))

(check-set-mode! 'report-failed)

;; 1. 字符串键与符号键均可读取
(let ((obj '(("name" . "kimi") (id . "kimi-coding"))))
  (check (pi-user-dict-ref obj "name" #f) => "kimi")
  (check (pi-user-dict-ref obj 'id #f) => "kimi-coding")
) ;let

;; 2. 缺失键返回默认值
(check (pi-user-dict-ref '() "missing" "fallback") => "fallback")
(check (pi-user-dict-ref #f "name" #f) => #f)

(check-report)
