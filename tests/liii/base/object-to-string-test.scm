(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; object->string
;; 将对象转换为字符串表示。
;;
;; 语法
;; ----
;; (object->string obj)
;; (object->string obj write)
;; (object->string obj write max-len)
;;
;; 参数
;; ----
;; obj : any?
;; 要转换为字符串的对象。
;;
;; write : boolean? 或 :readable，可选，默认为 #t
;; 如果为 #t，使用 write 风格（可被 read 解析）；
;; 如果为 :readable，生成可读的字符串表示。
;;
;; max-len : integer?，可选，默认为很大的数
;; 输出字符串的最大长度。
;;
;; 返回值
;; ------
;; string?
;; 对象的字符串表示。


;; 测试基本类型
(check (object->string 42) => "42")
(check (object->string "hello")
  =>
  "\"hello\""
) ;check
(check (object->string 'symbol)
  =>
  "symbol"
) ;check
(check (object->string #t) => "#t")
(check (object->string #f) => "#f")


;; 测试列表
(check (object->string '(1 2 3))
  =>
  "(1 2 3)"
) ;check
(check (object->string '()) => "()")


;; 测试向量
(check (object->string #(1 2 3))
  =>
  "#(1 2 3)"
) ;check


;; 测试过程
(check (string? (object->string (lambda (x) x))
       ) ;string?
  =>
  #t
) ;check


;; 测试 write 参数为 #f（使用 display 风格）
(check (object->string "hello" #f)
  =>
  "hello"
) ;check
(check (object->string 42 #f) => "42")


;; 测试嵌套结构
(check (object->string '(1 (2 3) 4))
  =>
  "(1 (2 3) 4)"
) ;check


(check-report)
