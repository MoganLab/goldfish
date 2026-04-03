;; (liii argparse) 模块函数分类索引
;;
;; argparse 用于解析命令行参数，支持长选项（--name）、短选项（-n）和默认值。

;; ==== 常见用法示例 ====
(import (liii argparse))

;; 示例1：创建一个解析器并添加字符串参数
(define parser (make-argument-parser))
(parser :add-argument '((name . "name") (type . string) (short . "n") (default . "anonymous")))
(parser :parse-args '("--name" "john"))
(parser 'name) ; => "john"

;; 示例2：添加数值类型参数
(define parser2 (make-argument-parser))
(parser2 :add-argument '((name . "width") (type . number) (short . "w") (default . 80)))
(parser2 :parse-args '("-w" "100"))
(parser2 'width) ; => 100

;; 示例3：使用 :get-argument 获取参数值
(define parser3 (make-argument-parser))
(parser3 :add-argument '((name . "title") (type . string) (default . "Untitled")))
(parser3 :get-argument "title") ; => "Untitled"

;; ==== 函数分类索引 ====

;; 一、解析器创建
;; 用于创建命令行参数解析器的函数
;;   make-argument-parser  - 创建一个新的参数解析器实例

;; 二、参数定义
;; 用于向解析器添加参数选项
;;   :add-argument         - 添加一个参数定义（也可用 :add）
;;                         - 参数: '((name . "argname") (type . string/number) (short . "s") (default . value))

;; 三、参数解析
;; 用于解析命令行参数
;;   :parse-args           - 解析参数列表（也可用 :parse）
;;                         - 不传参数时默认使用 (cddr (argv))

;; 四、参数获取
;; 用于获取解析后的参数值
;;   :get-argument         - 按名称获取参数值（也可用 :get）
;;   直接调用语法         - (parser 'argname) 快捷获取参数值

;; ==== 单元测试 ====

(import (liii check)
        (liii argparse)
        (liii base)
) ;import

(check-set-mode! 'report-failed)

(let ((parser (make-argument-parser)))
  (parser :add-argument
    '((name . "name") (type . string) (short . "n") (default . "anonymous"))
  ) ;parser
  (check (parser 'name) => "anonymous")
  (parser :parse-args '("--name" "john"))
  (check (parser 'name) => "john")
) ;let

(let ((parser (make-argument-parser)))
  (parser :add-argument
    '((name . "width") (type . number) (short . "width") (default . 80))
  ) ;parser

  (check (parser :get-argument "width") => 80)

  (parser :parse-args '("--width" "100"))
  (check (parser :get-argument "width") => 100)
  (check (parser 'width) => 100)

  (parser :parse-args '("-width" "60"))
  (check (parser 'width) => 60)
) ;let

(let ((parser (make-argument-parser)))
  (parser :add-argument
    '((name . "height") (type . number) (default . 60))  ; without short name
  ) ;parser
  (parser :parse-args '("--height" "120"))
  (check (parser :get-argument "height") => 120)
) ;let

(let ((parser (make-argument-parser)))
  (parser :add-argument
    '((name . "width") (type . number) (short . "w") (default . 80))
  ) ;parser
  (parser :add-argument
    '((name . "title") (type . string) (default . "Untitled"))
  ) ;parser
  (parser :parse-args '("-w" "100" "--title" "My Document"))
  (check (parser :get-argument "width") => 100)
  (check (parser :get-argument "title") => "My Document")
) ;let

(let ((parser (make-argument-parser)))
  (check-catch 'type-error (parser :add-argument '((name name))))
  (check-catch 'value-error (parser :add-argument '()))
) ;let

(check-report)

