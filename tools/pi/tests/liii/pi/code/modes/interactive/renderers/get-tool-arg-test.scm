;; get-tool-arg
;; 从工具参数列表中按键名提取对应的参数值。
;;
;; 语法
;; ----
;; (get-tool-arg key args)
;; (get-tool-arg key args default)
;;
;; 参数
;; ----
;; key : symbol? or string?
;; 待查找的参数键名。
;; args : list?
;; 工具参数 alist。
;; default : any
;; 键不存在时的默认返回值，缺省为 #f。
;;
;; 返回值
;; ----
;; any
;; 查找到的参数值或默认值。

(import (scheme base) (liii check) (liii pi code modes interactive renderers))

(check-set-mode! 'report-failed)

(define sample-args '((command . "ls -la") ("path" . "/tmp") (:timeout . 5)))

;; 1. 查找 symbol 或 string 键
(check (get-tool-arg 'command sample-args) => "ls -la")
(check (get-tool-arg "command" sample-args) => "ls -la")
(check (get-tool-arg 'path sample-args) => "/tmp")
(check (get-tool-arg "path" sample-args) => "/tmp")

;; 2. 键不存在时返回默认值
(check (get-tool-arg 'not-exist sample-args) => #f)
(check (get-tool-arg 'not-exist sample-args "fallback") => "fallback")

;; 3. 非 list 参数安全返回默认值
(check (get-tool-arg 'key "not-a-list" 123) => 123)

(check-report)
