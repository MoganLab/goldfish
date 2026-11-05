;; %normalize-cwd
;; 校验并归一化工作目录参数。
;;
;; 语法
;; ----
;; (%normalize-cwd caller cwd)
;;
;; 参数
;; ----
;; caller : symbol 或 string
;; 调用方名称，用于错误提示信息。
;;
;; cwd : any
;; 待校验的工作目录对象（#f、path 或 string）。
;;
;; 返回值
;; ------
;; string 或 #f
;; 归一化后的工作目录路径字符串，或者 #f。
;;
;; 说明
;; ----
;; - 若 cwd 为 #f，返回 #f。
;; - 若 cwd 为 path 对象，转换为其字符串路径表示。
;; - 若 cwd 为 string，直接返回原字符串。
;; - 若 cwd 为其他类型，抛出 type-error。

(import (liii check) (liii path) (liii subprocess process))

(check-set-mode! 'report-failed)

;; cwd 为 #f 时返回 #f
(check (%normalize-cwd 'spawn #f) => #f)

;; string 直接原样返回
(check (%normalize-cwd 'spawn "/var/log") => "/var/log")
(check (%normalize-cwd 'spawn ".") => ".")

;; path 对象转换为 string
(check (%normalize-cwd 'spawn (path "/tmp")) => "/tmp")
(check (%normalize-cwd 'spawn (path ".")) => ".")

;; 其他非法类型抛出 type-error
(check-catch 'type-error (%normalize-cwd 'spawn 123))
(check-catch 'type-error (%normalize-cwd 'spawn 'dir))
(check-catch 'type-error (%normalize-cwd 'spawn '("/tmp")))
(check-catch 'type-error (%normalize-cwd 'spawn #t))

(check-report)
