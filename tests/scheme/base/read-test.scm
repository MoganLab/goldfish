(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; read
;; 从输入端口读取一个S表达式，根据R7RS规范，read函数用于从给定的输入端口读取Scheme数据。
;;
;; 语法
;; ----
;; (read)
;; (read port)
;;
;; 参数
;; ----
;; port : port? (可选)
;; 输入端口，如果未提供则使用当前输入端口
;;
;; 返回值
;; -----
;; any?
;; 返回从端口读取的S表达式，到达文件末尾时返回EOF对象
;;
;; 描述
;; ----
;; read函数从指定的输入端口读取一个完整的S表达式。如果未提供端口参数，
;; 则从当前输入端口读取。读取过程会正确处理各种Scheme数据类型，包括
;; 基本类型（数字、字符串、符号、布尔值）和复合类型（列表、向量等）。
;;
;; 行为特征
;; ------
;; - 读取完整的S表达式，包括嵌套结构
;; - 正确处理引号、反引号等特殊语法
;; - 支持所有Scheme数据类型的读取
;; - 到达文件末尾时返回EOF对象
;; - 输入格式错误会抛出读取错误
;;
;; 错误处理
;; ------
;; - 输入格式错误：抛出读取错误
;; - 端口错误：如果端口无效会抛出相应错误
;; - 内存不足：可能抛出内存错误
;;
;; 与write的关系
;; ------------
;; read函数与write函数配合使用可以实现数据的序列化和反序列化：
;; (write data port) 写入的数据可以通过 (read port) 重新读取
;;
;; 跨平台行为
;; ---------
;; - 字符编码：支持UTF-8编码的文本输入
;; - 数字格式：支持各种数字字面量格式
;; - 字符串转义：正确处理转义字符
;; 基本数据类型读取测试
(let ((port (open-input-string "123")))
  (check (read port) => 123)
) ;let
(let ((port (open-input-string "-456")))
  (check (read port) => -456)
) ;let
(let ((port (open-input-string "3.14")))
  (check (read port) => 3.14)
) ;let
(let ((port (open-input-string "\"hello world\"")
      ) ;port
     ) ;
  (check (read port) => "hello world")
) ;let
(let ((port (open-input-string "hello")))
  (check (read port) => 'hello)
) ;let
(let ((port (open-input-string "#t")))
  (check (read port) => #t)
) ;let
(let ((port (open-input-string "#f")))
  (check (read port) => #f)
) ;let
;; 列表读取测试
(let ((port (open-input-string "(1 2 3)")))
  (check (read port) => '(1 2 3))
) ;let
(let ((port (open-input-string "(a b c)")))
  (check (read port) => '(a b c))
) ;let
(let ((port (open-input-string "(1 \"two\" 3)")
      ) ;port
     ) ;
  (check (read port) => '(1 "two" 3))
) ;let
;; 嵌套列表读取测试
(let ((port (open-input-string "(1 (2 3) 4)"))
     ) ;
  (check (read port) => '(1 (2 3) 4))
) ;let
(let ((port (open-input-string "((a b) (c d))")
      ) ;port
     ) ;
  (check (read port) => '((a b) (c d)))
) ;let
;; 向量读取测试
(let ((port (open-input-string "#(1 2 3)")))
  (check (read port) => #(1 2 3))
) ;let
(let ((port (open-input-string "#(a \"b\" c)")
      ) ;port
     ) ;
  (check (read port) => #(a "b" c))
) ;let
;; u8 vector (bytevector) 读取测试
(let ((port (open-input-string "#u8(1 2 3)"))
     ) ;
  (check (read port) => #u8(1 2 3))
) ;let
(let ((port (open-input-string "#u8(0 255 127)")
      ) ;port
     ) ;
  (check (read port) => #u8(0 255 127))
) ;let
(let ((port (open-input-string "#u8()")))
  (check (read port) => #u())
) ;let
(let ((port (open-input-string "#u(1 2 3)")))
  (check (read port) => #u8(1 2 3))
) ;let
;; 引号语法测试
(let ((port (open-input-string "'hello")))
  (check (read port) => ''hello)
) ;let
(let ((port (open-input-string "'(1 2 3)")))
  (check (read port) => ''(1 2 3))
) ;let
(let ((port (open-input-string "`hello")))
  (check (read port) => ''hello)
) ;let
;; 取消引号语法 - 这些在当前实现中可能不支持
;; (let ((port (open-input-string ",hello")))
;;   (check (read port) => ',hello))
;; (let ((port (open-input-string ",@hello")))
;;   (check (read port) => ',@hello))
;; 复杂表达式测试
(let ((port (open-input-string "(+ 1 2 3)")))
  (check (read port) => '(+ 1 2 3))
) ;let
(let ((port (open-input-string "(define x 42)")
      ) ;port
     ) ;
  (check (read port) => '(define x 42))
) ;let
(let ((port (open-input-string "(if #t yes no)")
      ) ;port
     ) ;
  (check (read port) => '(if #t yes no))
) ;let
;; 空列表测试
(let ((port (open-input-string "()")))
  (check (read port) => '())
) ;let
;; 布尔值列表测试
(let ((port (open-input-string "(#t #f #t)"))
     ) ;
  (check (read port) => '(#t #f #t))
) ;let
;; 混合类型列表测试
(let ((port (open-input-string "(1 \"two\" 'three 4.0)"
            ) ;open-input-string
      ) ;port
     ) ;
  (check (read port)
    =>
    '(1 "two" 'three 4.0)
  ) ;check
) ;let
;; 文件结束测试
(let ((port (open-input-string "")))
  (check (eof-object? (read port)) => #t)
) ;let
(let ((port (open-input-string "123")))
  (check (read port) => 123)
  (check (eof-object? (read port)) => #t)
) ;let
;; 多个表达式测试
(let ((port (open-input-string "123 456 \"hello\"")
      ) ;port
     ) ;
  (check (read port) => 123)
  (check (read port) => 456)
  (check (read port) => "hello")
  (check (eof-object? (read port)) => #t)
) ;let
;; 注释处理测试（如果支持）
(let ((port (open-input-string "123 ; this is a comment\n456"
            ) ;open-input-string
      ) ;port
     ) ;
  (check (read port) => 123)
  (check (read port) => 456)
) ;let
;; 空白字符处理测试
(let ((port (open-input-string "   123   456   ")
      ) ;port
     ) ;
  (check (read port) => 123)
  (check (read port) => 456)
) ;let
;; 换行符处理测试
(let ((port (open-input-string "123\n456\n789")
      ) ;port
     ) ;
  (check (read port) => 123)
  (check (read port) => 456)
  (check (read port) => 789)
) ;let
;; 与write联动测试
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
;; 错误处理测试 - 不完整的表达式
;; (let ((port (open-input-string "(1 2")))
;;   (check-catch 'read-error (read port)))
;; 大数字测试
(let ((port (open-input-string "12345678901234567890"
            ) ;open-input-string
      ) ;port
     ) ;
  (check-true (number? (read port)))
) ;let
;; 特殊符号测试
(let ((port (open-input-string "hello-world hello_world hello.world"
            ) ;open-input-string
      ) ;port
     ) ;
  (check (read port) => 'hello-world)
  (check (read port) => 'hello_world)
  (check (read port) => 'hello.world)
) ;let
;; 中文符号测试
(let ((port (open-input-string "'中文测试")
      ) ;port
     ) ;
  (check (read port) => ''中文测试)
) ;let
;; 嵌套引号测试
(let ((port (open-input-string "''hello")))
  (check (read port) => '''hello)
) ;let
;; 复杂嵌套测试
(let ((port (open-input-string "(a (b (c d)) e)")
      ) ;port
     ) ;
  (check (read port) => '(a (b (c d)) e))
) ;let
;; 向量嵌套测试
(let ((port (open-input-string "#(1 #(2 3) 4)")
      ) ;port
     ) ;
  (check (read port) => #(1 #(2 3) 4))
) ;let
;; 当前输入端口测试（需要重定向）
;; (let ((original-input (current-input-port)))
;;   (let ((string-port (open-input-string "42")))
;;     (set-current-input-port! string-port)
;;     (check (read) => 42)
;;     (set-current-input-port! original-input)))
(check-report)