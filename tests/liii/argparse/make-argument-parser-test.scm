;; make-argument-parser
;; 创建命令行参数解析器（支持长选项 --name、短选项 -n、默认值与多种动作）。
;;
;; 语法
;; ----
;; (make-argument-parser)
;; (make-argument-parser options)
;;
;; 参数
;; ----
;; options : alist (可选，默认 '())
;; 解析器级配置项：
;;   (command . "fmt")              子命令名，解析时该参数被跳过
;;   (skip-value-options "-m" ...)  跳过且消费其后取值的选项
;;   (skip-prefix-options "-m=" ...)  按前缀跳过的选项
;;   (unknown-options . error)      未知选项处理：error / ignore / positional
;;
;; 返回值
;; ----
;; procedure
;; 解析器过程，接收命令符号与可选参数，返回对应结果。
;; 支持的命令：
;;   (parser :add-argument '((name . "n") (type . string) ...))  添加参数定义
;;   (parser :parse-args '("--name" "v"))                        解析参数列表
;;   (parser :parse-argv '("bin/gf" "fmt"))                      解析完整命令行
;;   (parser :get-argument "name")                               按名称取值
;;   (parser :positionals)                                       取位置参数
;;   (parser 'name)                                              按名称取值的快捷写法
;;
;; 说明
;; ----
;; 1. 参数定义支持 (name . "...")、(type . string|number)、(short . "...")、
;;    (default . value)、(action . store|store-true|store-false)。
;; 2. store-true / store-false 为布尔开关，不接受行内取值。
;; 3. 未注册参数默认抛出错误，可用 unknown-options 配置忽略或降级为位置参数。
;;
;; 错误处理
;; ----
;; type-error
;; name / short 非字符串，或 type / action 非符号时抛出。
;;
;; value-error
;; type 不是 string/number、action 不是 store/store-true/store-false、
;; 缺少 name、遇到未知选项时抛出。

(import (liii check) (liii argparse) (liii base))

(check-set-mode! 'report-failed)

;; 基本用法：默认值与长选项解析
(let ((parser (make-argument-parser)))
  (parser :add-argument
    '((name . "name") (type . string) (short . "n") (default . "anonymous"))
  ) ;parser
  (check (parser 'name) => "anonymous")
  (parser :parse-args '("--name" "john"))
  (check (parser 'name) => "john")
) ;let

;; 数值类型参数：长选项、短选项与取值转换
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

;; 无 short 名称的参数
(let ((parser (make-argument-parser)))
  (parser :add-argument '((name . "height") (type . number) (default . 60)))
  (parser :parse-args '("--height" "120"))
  (check (parser :get-argument "height") => 120)
) ;let

;; 多个参数共存
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

;; 参数定义校验
(let ((parser (make-argument-parser)))
  (check-catch 'type-error (parser :add-argument '((name name))))
  (check-catch 'value-error (parser :add-argument '()))
) ;let

;; store-true / store-false 布尔开关
(let ((parser (make-argument-parser)))
  (parser :add-argument '((name . "verbose")
                          (short . "v")
                          (action . store-true)))
  (parser :add-argument '((name . "cache") (action . store-false)))
  (check (parser 'verbose) => #f)
  (check (parser 'cache) => #t)
  (parser :parse-args '("--verbose" "--cache"))
  (check (parser 'verbose) => #t)
  (check (parser 'cache) => #f)
) ;let

;; 行内取值（--width=100、-t=Doc）与位置参数
(let ((parser (make-argument-parser)))
  (parser :add-argument
    '((name . "width") (type . number) (short . "w") (default . 80))
  ) ;parser
  (parser :add-argument
    '((name . "title") (type . string) (short . "t") (default . "Untitled"))
  ) ;parser
  (parser :parse-args '("input.scm" "--width=100" "-t=Doc" "output.scm"))
  (check (parser 'width) => 100)
  (check (parser 'title) => "Doc")
  (check (parser :positionals) => '("input.scm" "output.scm"))
) ;let

;; 子命令、跳过取值选项与未知选项忽略
(let ((parser (make-argument-parser '((command . "fmt")
                                      (skip-value-options "-m"
                                        "--mode"
                                        "-I"
                                        "-A")
                                      (skip-prefix-options "-m=" "--mode=")
                                      (unknown-options . ignore))
              ) ;make-argument-parser
      ) ;parser
     ) ;
  (parser :add-argument '((name . "dry-run") (action . store-true)))
  (parser :parse-argv '("bin/gf" "-m" "r7rs" "fmt" "--dry-run" "file.scm"))
  (check (parser 'dry-run) => #t)
  (check (parser :get-positionals) => '("file.scm"))
) ;let

;; 未知选项降级为位置参数
(let ((parser (make-argument-parser '((command . "fmt")
                                      (skip-value-options "-m"
                                        "--mode"
                                        "-I"
                                        "-A")
                                      (skip-prefix-options "-m=" "--mode=")
                                      (unknown-options . positional))
              ) ;make-argument-parser
      ) ;parser
     ) ;
  (parser :add-argument '((name . "dry-run") (action . store-true)))
  (parser :parse-argv '("bin/gf" "fmt" "--dryrun" "file.scm"))
  (check (parser 'dry-run) => #f)
  (check (parser :positionals) => '("--dryrun" "file.scm"))
) ;let

;; 未知选项默认报错
(let ((parser (make-argument-parser)))
  (check-catch 'value-error (parser :parse-args '("--bad")))
) ;let

;; unknown-options 为 ignore 时未知选项被丢弃
(let ((parser (make-argument-parser '((unknown-options . ignore)))))
  (parser :parse-args '("--bad" "file.scm"))
  (check (parser :positionals) => '("file.scm"))
) ;let

;; unknown-options 为 positional 时未知选项进入位置参数
(let ((parser (make-argument-parser '((unknown-options . positional)))))
  (parser :parse-args '("--bad" "file.scm"))
  (check (parser :positionals) => '("--bad" "file.scm"))
) ;let

(check-report)
