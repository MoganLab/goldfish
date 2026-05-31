(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; format
;; 按照控制字符串格式化参数并返回结果字符串或输出到端口。
;;
;; 语法
;; ----
;; (format port control-string . args)
;;
;; 参数
;; ----
;; port          - #f 表示返回字符串，#t 表示输出到 current-output-port，
;;                 或者一个输出端口
;; control-string - 包含格式指令的字符串
;; args          - 要格式化的参数
;;
;; 返回值
;; ------
;; 当 port 为 #f 时返回格式化后的字符串，否则返回空字符串。
;;
;; 说明
;; ----
;; format 是 S7 内置函数，支持 Common Lisp 风格的格式指令。
;; 常用指令包括：
;;   ~A - 任意对象的文本表示（不引号）
;;   ~S - 任意对象的 Scheme 表示（字符串加引号）
;;   ~C - 字符
;;   ~D - 十进制整数
;;   ~X - 十六进制整数
;;   ~B - 二进制整数
;;   ~O - 八进制整数
;;   ~% - 换行符
;;   ~~ - 字面量 ~
;;
;; 示例
;; ----
;; (format #f "hello")              => "hello"
;; (format #f "~A" 123)             => "123"
;; (format #f "~S" "abc")           => "\"abc\""
;; (format #f "~A ~A" 1 2)          => "1 2"
;; (format #f "~%")                  => "\n"
;; (format #f "~~")                  => "~"
;; (format #f "~C" #\a)             => "a"
;; (format #f "~D" 42)              => "42"
;; (format #f "~X" 255)             => "ff"
;; (format #f "~B" 5)               => "101"
;; (format #f "~O" 8)               => "10"


(check (format #f "hello") => "hello")
(check (format #f "~A" 123) => "123")
(check (format #f "~S" "abc") => "\"abc\"")
(check (format #f "~A ~A" 1 2) => "1 2")
(check (format #f "~%") => "\n")
(check (format #f "~~") => "~")
(check (format #f "~C" #\a) => "a")
(check (format #f "~D" 42) => "42")
(check (format #f "~X" 255) => "ff")
(check (format #f "~B" 5) => "101")
(check (format #f "~O" 8) => "10")

(check-report)
