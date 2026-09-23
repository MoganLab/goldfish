(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; format
;; 按照格式化字符串格式化参数并返回结果字符串或输出到端口。
;; 支持 SRFI-48 规范，允许省略目标端口参数（默认为 #f 并返回字符串）。
;;
;; 语法
;; ----
;; (format [port] format-string . args)
;;
;; 参数
;; ----
;; port          - 可选。#f 表示返回字符串，#t 表示输出到 current-output-port，
;;                 或者一个输出端口。若省略 port 则默认目标端口为 #f 并返回格式化结果字符串。
;; format-string - 包含格式指令的字符串
;; args          - 要格式化的参数
;;
;; 返回值
;; ------
;; 当省略 port 或 port 为 #f 时返回格式化后的字符串；
;; 当 port 为输出端口或 #t 时向相应端口输出。
;;
;; 说明
;; ----
;; format 支持 SRFI-48 以及 Common Lisp 风格的核心格式指令。
;; 核心指令包括：
;;   ~A, ~a - 任意对象的文本表示（不加引号）
;;   ~S, ~s - 任意对象的 Scheme 表示（字符串加引号）
;;   ~C, ~c - 字符
;;   ~D, ~d - 十进制整数
;;   ~B, ~b - 二进制整数
;;   ~O, ~o - 八进制整数
;;   ~X, ~x - 十六进制整数
;;   ~F, ~f - 浮点数表示
;;   ~E, ~e - 科学计数法表示
;;   ~%     - 换行符
;;   ~&     - 新行（若前序字符非换行符则换行）
;;   ~~     - 字面量 ~
;;
;; 示例
;; ----
;; (format "hello")                 => "hello"
;; (format "~a" 123)                => "123"
;; (format "~s" "abc")              => "\"abc\""
;; (format "~d" 42)                 => "42"
;; (format "~b" 5)                  => "101"
;; (format "~o" 8)                  => "10"
;; (format "~x" 255)                => "ff"
;; (format "~c" #\a)                => "a"
;; (format "~f" 1.5)                => "1.500000"
;; (format "~e" 100.1)              => "1.001000e+02"
;; (format "~%")                    => "\n"
;; (format "~~")                    => "~"
;; (format "hello~&world")          => "hello\nworld"
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


;; 省略 port 参数的测试场景（SRFI-48 核心特性）
(check (format "hello") => "hello")
(check (format "hello~%") => "hello\n")
(check (format "~a" 123) => "123")
(check (format "~a" "abc") => "abc")
(check (format "~A" 123) => "123")
(check (format "~s" "abc") => "\"abc\"")
(check (format "~S" "abc") => "\"abc\"")
(check (format "~a ~s" "hello" "world") => "hello \"world\"")
(check (format "~d" 42) => "42")
(check (format "~d" -17) => "-17")
(check (format "~D" 100) => "100")
(check (format "~b" 5) => "101")
(check (format "~B" 12) => "1100")
(check (format "~o" 8) => "10")
(check (format "~O" 63) => "77")
(check (format "~x" 255) => "ff")
(check (format "~X" 255) => "ff")
(check (format "~c" #\a) => "a")
(check (format "~C" #\Z) => "Z")
(check (format "~f" 1.5) => "1.500000")
(check (format "~F" 100.1) => "100.100000")
(check (format "~e" 100.1) => "1.001000e+02")
(check (format "~E" 100.1) => "1.001000e+02")
(check (format "~%") => "\n")
(check (format "~~") => "~")
(check (format "~&") => "")
(check (format "hello~&world") => "hello\nworld")
(check (format "hello~%~&world") => "hello\nworld")

;; 显式传递 port 为 #f 的原生 S7 行为测试
(check (format #f "hello") => "hello")
(check (format #f "hello~%") => "hello\n")
(check (format #f "~A" 123) => "123")
(check (format #f "~a" 123) => "123")
(check (format #f "~S" "abc") => "\"abc\"")
(check (format #f "~s" "abc") => "\"abc\"")
(check (format #f "~A ~A" 1 2) => "1 2")
(check (format #f "~%") => "\n")
(check (format #f "~~") => "~")
(check (format #f "~C" #\a) => "a")
(check (format #f "~c" #\Z) => "Z")
(check (format #f "~D" 42) => "42")
(check (format #f "~d" -17) => "-17")
(check (format #f "~X" 255) => "ff")
(check (format #f "~x" 255) => "ff")
(check (format #f "~B" 5) => "101")
(check (format #f "~b" 5) => "101")
(check (format #f "~O" 8) => "10")
(check (format #f "~o" 8) => "10")
(check (format #f "~f" 1.5) => "1.500000")
(check (format #f "~F" 100.1) => "100.100000")
(check (format #f "~e" 100.1) => "1.001000e+02")
(check (format #f "~E" 100.1) => "1.001000e+02")
(check (format #f "~&") => "")
(check (format #f "hello~&world") => "hello\nworld")

;; 端口输出测试
(let ((p (open-output-string)))
  (format p "hello ~a" 'world)
  (check (get-output-string p) => "hello world")
) ;let

(check (with-output-to-string (lambda () (format #t "test ~d" 123)))
  =>
  "test 123"
) ;check

;; 异常用例
(check-catch 'wrong-number-of-args (format))
(check-catch 'type-error (format 123 "hello"))

(check-report)
