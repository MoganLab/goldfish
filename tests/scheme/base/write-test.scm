(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; write
;; 向输出端口写入一个S表达式，根据R7RS规范，write函数用于将Scheme数据写入给定的输出端口。
;;
;; 语法
;; ----
;; (write obj)
;; (write obj port)
;;
;; 参数
;; ----
;; obj : any?
;; 要写入的Scheme对象
;; port : port? (可选)
;; 输出端口，如果未提供则使用当前输出端口
;;
;; 返回值
;; -----
;; unspecified
;; 返回值未指定，主要作用是副作用（向端口写入数据）
;;
;; 描述
;; ----
;; write函数将指定的Scheme对象以可读格式写入输出端口。如果未提供端口参数，
;; 则写入当前输出端口。输出的格式应该能被read函数正确读取，实现数据的
;; 序列化和反序列化。
;;
;; 行为特征
;; ------
;; - 输出完整的S表达式，包括嵌套结构
;; - 正确处理引号、反引号等特殊语法
;; - 支持所有Scheme数据类型的写入
;; - 字符串中的特殊字符会被正确转义
;; - 输出的格式注重可读性而非美观性
;;
;; 与display的区别
;; --------------
;; - write: 注重数据的可读性，输出格式能被read重新读取
;; - display: 注重人类可读性，输出更简洁的格式
;; - 例如：write输出字符串带引号，display可能不带
;;
;; 与read的关系
;; ------------
;; write函数与read函数配合使用可以实现数据的序列化和反序列化：
;; (write data port) 写入的数据可以通过 (read port) 重新读取
;;
;; 错误处理
;; ------
;; - 端口错误：如果端口无效会抛出相应错误
;; - IO错误：包括磁盘满、权限不足等
;; - 循环结构：需要特殊处理以避免无限递归
;;
;; 跨平台行为
;; ---------
;; - 字符编码：支持UTF-8编码的文本输出
;; - 数字格式：输出标准的数字字面量格式
;; - 字符串转义：正确处理需要转义的字符
;; 基本数据类型写入测试
(let ((port (open-output-string)))
  (write 123 port)
  (check (get-output-string port)
    =>
    "123"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write -456 port)
  (check (get-output-string port)
    =>
    "-456"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write 3.14 port)
  (check (get-output-string port)
    =>
    "3.14"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write "hello world" port)
  (check (get-output-string port)
    =>
    "\"hello world\""
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write 'hello port)
  (check (get-output-string port)
    =>
    "hello"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write #t port)
  (check (get-output-string port) => "#t")
) ;let
(let ((port (open-output-string)))
  (write #f port)
  (check (get-output-string port) => "#f")
) ;let
;; 列表写入测试
(let ((port (open-output-string)))
  (write '(1 2 3) port)
  (check (get-output-string port)
    =>
    "(1 2 3)"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write '(a b c) port)
  (check (get-output-string port)
    =>
    "(a b c)"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write '(1 "two" 3) port)
  (check (get-output-string port)
    =>
    "(1 \"two\" 3)"
  ) ;check
) ;let
;; 嵌套列表写入测试
(let ((port (open-output-string)))
  (write '(1 (2 3) 4) port)
  (check (get-output-string port)
    =>
    "(1 (2 3) 4)"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write '((a b) (c d)) port)
  (check (get-output-string port)
    =>
    "((a b) (c d))"
  ) ;check
) ;let
;; 向量写入测试
(let ((port (open-output-string)))
  (write #(1 2 3) port)
  (check (get-output-string port)
    =>
    "#(1 2 3)"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write #(a "b" c) port)
  (check (get-output-string port)
    =>
    "#(a \"b\" c)"
  ) ;check
) ;let
;; 引号语法写入测试
(let ((port (open-output-string)))
  (write ''hello port)
  (check (get-output-string port)
    =>
    "'hello"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write ''(1 2 3) port)
  (check (get-output-string port)
    =>
    "'(1 2 3)"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write ''hello port)
  (check (get-output-string port)
    =>
    "'hello"
  ) ;check
) ;let
;; 复杂表达式写入测试
(let ((port (open-output-string)))
  (write '(+ 1 2 3) port)
  (check (get-output-string port)
    =>
    "(+ 1 2 3)"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write '(define x 42) port)
  (check (get-output-string port)
    =>
    "(define x 42)"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write '(if #t yes no) port)
  (check (get-output-string port)
    =>
    "(if #t yes no)"
  ) ;check
) ;let
;; 空列表写入测试
(let ((port (open-output-string)))
  (write '() port)
  (check (get-output-string port) => "()")
) ;let
;; 布尔值列表写入测试
(let ((port (open-output-string)))
  (write '(#t #f #t) port)
  (check (get-output-string port)
    =>
    "(#t #f #t)"
  ) ;check
) ;let
;; 混合类型列表写入测试
(let ((port (open-output-string)))
  (write '(1 "two" 'three 4.0) port)
  (check (get-output-string port)
    =>
    "(1 \"two\" 'three 4.0)"
  ) ;check
) ;let
;; 字符串转义测试
(let ((port (open-output-string)))
  (write "hello\nworld" port)
  (check (get-output-string port)
    =>
    "\"hello\\nworld\""
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write "hello\tworld" port)
  (check (get-output-string port)
    =>
    "\"hello\\tworld\""
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write "hello\"world" port)
  (check (get-output-string port)
    =>
    "\"hello\\\"world\""
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write "hello\\world" port)
  (check (get-output-string port)
    =>
    "\"hello\\\\world\""
  ) ;check
) ;let
;; 特殊字符测试
(let ((port (open-output-string)))
  (write "\x00;\x01;\x02;" port)
  (check (get-output-string port)
    =>
    "\"\\x00;\\x01;\\x02;\""
  ) ;check
) ;let
;; 中文字符串写入测试
(let ((port (open-output-string)))
  (write "你好世界" port)
  (check (get-output-string port)
    =>
    "\"你好世界\""
  ) ;check
) ;let
;; 大数字写入测试
(let ((port (open-output-string)))
  (write -6101065172474983726 port)
  (check-true (string? (get-output-string port))
  ) ;check-true
) ;let
;; 分数写入测试
(let ((port (open-output-string)))
  (write 1/2 port)
  (check (get-output-string port)
    =>
    "1/2"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write -22/7 port)
  (check (get-output-string port)
    =>
    "-22/7"
  ) ;check
) ;let
;; 复数写入测试
(let ((port (open-output-string)))
  (write 1.0+2.0i port)
  (check (get-output-string port)
    =>
    "1.0+2.0i"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write 3.14-2.71i port)
  (check (get-output-string port)
    =>
    "3.14-2.71i"
  ) ;check
) ;let
;; 嵌套向量测试
(let ((port (open-output-string)))
  (write #(1 #(2 3) 4) port)
  (check (get-output-string port)
    =>
    "#(1 #(2 3) 4)"
  ) ;check
) ;let
;; 深层嵌套测试
(let ((port (open-output-string)))
  (write '(a (b (c d)) e) port)
  (check (get-output-string port)
    =>
    "(a (b (c d)) e)"
  ) ;check
) ;let
;; 与read联动测试 - 确保write的输出能被read正确读取
(let ((output-port (open-output-string)))
  (write '(1 2 3) output-port)
  (let ((input-port (open-input-string (get-output-string output-port)
                    ) ;open-input-string
        ) ;input-port
       ) ;
    (check (read input-port) => '(1 2 3))
  ) ;let
) ;let
(let ((output-port (open-output-string)))
  (write "hello world" output-port)
  (let ((input-port (open-input-string (get-output-string output-port)
                    ) ;open-input-string
        ) ;input-port
       ) ;
    (check (read input-port)
      =>
      "hello world"
    ) ;check
  ) ;let
) ;let
(let ((output-port (open-output-string)))
  (write 123.456 output-port)
  (let ((input-port (open-input-string (get-output-string output-port)
                    ) ;open-input-string
        ) ;input-port
       ) ;
    (check (read input-port) => 123.456)
  ) ;let
) ;let
(let ((output-port (open-output-string)))
  (write #(#t #f "hello") output-port)
  (let ((input-port (open-input-string (get-output-string output-port)
                    ) ;open-input-string
        ) ;input-port
       ) ;
    (check (read input-port)
      =>
      #(#t #f "hello")
    ) ;check
  ) ;let
) ;let
;; 多个值写入测试
(let ((port (open-output-string)))
  (write 123 port)
  (write 456 port)
  (write "hello" port)
  (check (get-output-string port)
    =>
    "123456\"hello\""
  ) ;check
) ;let
;; 当前输出端口测试（需要重定向）
;; (let ((original-output (current-output-port)))
;;   (let ((string-port (open-output-string)))
;;     (set-current-output-port! string-port)
;;     (write 42)
;;     (check (get-output-string string-port) => "42")
;;     (set-current-output-port! original-output)))
;; 特殊符号写入测试
(let ((port (open-output-string)))
  (write 'hello-world port)
  (check (get-output-string port)
    =>
    "hello-world"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write 'hello_world port)
  (check (get-output-string port)
    =>
    "hello_world"
  ) ;check
) ;let
(let ((port (open-output-string)))
  (write 'hello.world port)
  (check (get-output-string port)
    =>
    "hello.world"
  ) ;check
) ;let
;; 中文符号写入测试
(let ((port (open-output-string)))
  (write '中文测试 port)
  (check (get-output-string port)
    =>
    "中文测试"
  ) ;check
) ;let
;; 嵌套引号写入测试
(let ((port (open-output-string)))
  (write '''hello port)
  (check (get-output-string port)
    =>
    "''hello"
  ) ;check
) ;let
;; 空字符串测试
(let ((port (open-output-string)))
  (write "" port)
  (check (get-output-string port)
    =>
    "\"\""
  ) ;check
) ;let
;; 单字符字符串测试
(let ((port (open-output-string)))
  (write "a" port)
  (check (get-output-string port)
    =>
    "\"a\""
  ) ;check
) ;let
;; 数值边界测试
(let ((port (open-output-string)))
  (write 0 port)
  (check (get-output-string port) => "0")
) ;let
(let ((port (open-output-string)))
  (write -0.0 port)
  (check (get-output-string port)
    =>
    "-0.0"
  ) ;check
) ;let
;; 精确与非精确数测试
(let ((port (open-output-string)))
  (write 42 port)
  (check (get-output-string port) => "42")
) ;let
(let ((port (open-output-string)))
  (write 42.0 port)
  (check (get-output-string port)
    =>
    "42.0"
  ) ;check
) ;let
;; 复杂嵌套结构测试
(let ((port (open-output-string)))
  (write '((1 2) (3 4) (5 (6 7))) port)
  (check (get-output-string port)
    =>
    "((1 2) (3 4) (5 (6 7)))"
  ) ;check
) ;let
;; 向量和列表混合测试
(let ((port (open-output-string)))
  (write '(#(1 2) #(3 4) 5) port)
  (check (get-output-string port)
    =>
    "(#(1 2) #(3 4) 5)"
  ) ;check
) ;let
;; 长列表测试
(let ((port (open-output-string)))
  (write '(1 2 3 4 5 6 7 8 9 10) port)
  (check (get-output-string port)
    =>
    "(1 2 3 4 5 6 7 8 9 10)"
  ) ;check
) ;let
;; 多维向量测试
(let ((port (open-output-string)))
  (write #(#(1 2) #(3 4)) port)
  (check (get-output-string port)
    =>
    "#(#(1 2) #(3 4))"
  ) ;check
) ;let
(check-report)
(check-report)