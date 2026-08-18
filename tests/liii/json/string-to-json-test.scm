(import (liii check) (liii json) (liii base) (liii error))

(check-set-mode! 'report)

;; string->json
;; 将 JSON 字符串解析为 Scheme 数据结构。
;;
;; 语法
;; ----
;; (string->json json-string)
;;
;; 参数
;; ----
;; json-string : string?
;; 要解析的 JSON 字符串。
;;
;; 返回值
;; ----
;; any?
;; 返回对应的对象、数组、字符串、数字、布尔值、null 或 eof-object。
;;
;; 注意
;; ----
;; 支持宽松对象键语法，以及 Unicode 转义与代理对解析。
;;
;; 示例
;; ----
;; (string->json "[1,2,3]") => #(1 2 3)
;; (string->json "{a:{b:1,c:2}}") => '((a . ((b . 1) (c . 2))))
;;
;; 错误处理
;; ----
;; parse-error 当字符串中存在非法转义或非法 Unicode 序列时。
;; read-error 当输入不完整时。

(check (string->json "{\"name\":\"Bob\",\"age\":21}")
  =>
  '(("name" . "Bob") ("age" . 21))
) ;check
(check (string->json "[1,2,3]") => #(1 2 3))
(check (string->json "[]") => #())
(check (string->json "[true]") => #(true))
(check (string->json "[false]") => #(false))
(check (string->json "[{data: 1},{}]") => #(((data . 1)) (())))
(check (string->json "{}") => '(()))
(check (string->json "{args: {}}") => '((args ())))
(check (string->json "{\"args\": {}}") => '(("args" ())))
(check (string->json "{\"args\": {}, data: 1}") => '(("args" ()) (data . 1)))
(check (string->json "{\"args\": {}, data: [1,2,3]}")
  =>
  '(("args" ()) (data . #(1 2 3)))
) ;check
(check (string->json "{\"args\": {}, data: true}")
  =>
  '(("args" ()) (data . true))
) ;check
(check (string->json "{\"args\": {}, data: null}")
  =>
  '(("args" ()) (data . null))
) ;check
(check (string->json "{a:{b:1,c:2}}") => '((a (b . 1) (c . 2))))

(check (string->json "{\"age\":18}") => '(("age" . 18)))
(check (string->json "{age:18}") => '((age . 18)))
(check (string->json "{\"name\":\"中文\"}") => '(("name" . "中文")))
(check (string->json "{\"name\":\"Alice\\nBob\"}")
  =>
  '(("name" . "Alice\nBob"))
) ;check
(check (string->json "{\"name\":\"Alice\\tBob\"}")
  =>
  '(("name" . "Alice\tBob"))
) ;check
(check (string->json "{\"name\":\"Alice\\rBob\"}")
  =>
  '(("name" . "Alice\rBob"))
) ;check
(check (string->json "{\"name\":\"Alice\\bBob\"}")
  =>
  '(("name" . "Alice\bBob"))
) ;check
(check (string->json "{\"name\":\"Alice\\fBob\"}")
  =>
  '(("name" . "Alice\fBob"))
) ;check
(check (string->json "{\"name\":\"Alice\\\\Bob\"}")
  =>
  '(("name" . "Alice\\Bob"))
) ;check
(check (string->json "{\"name\":\"Alice\\/Bob\"}") => '(("name" . "Alice/Bob")))
(check (string->json "{\"name\":\"Alice\\\"Bob\"}")
  =>
  '(("name" . "Alice\"Bob"))
) ;check
(check (string->json "[\"\\u0041\"]") => #("A"))
(check (string->json "[\"\\u0041\\u0042\"]") => #("AB"))
(check (string->json "[\"\\u4E2D\\u6587\"]") => #("中文"))
(check (string->json "[\"\\uD83D\\uDE00\"]") => #("😀"))
(check (string->json "{\"name\":\"\\u4E2D\\u6587\"}") => '(("name" . "中文")))
(check (string->json "{\"emoji\":\"\\uD83D\\uDE00\"}") => '(("emoji" . "😀")))
(check-catch 'parse-error (string->json "[\"\\u004G\"]"))
(check-catch 'parse-error (string->json "[\"\\a\"]"))
(check (string->json "") => (eof-object))
(check-catch 'parse-error (string->json "."))
(check-catch 'parse-error (string->json "["))

;;; 顶层标量：[0125] 严格 parser 直接返回标量值
(check (string->json "42") => 42)
(check (string->json "-7") => -7)
(check (string->json "3.14") => 3.14)
(check (string->json "true") => 'true)
(check (string->json "null") => 'null)
(check (string->json "\"hello\"") => "hello")

;;; 空白字符容忍
(check (string->json "  [1, 2 , 3]  ") => #(1 2 3))
(check (string->json "{ \"a\" : 1 , \"b\" : 2 }") => '(("a" . 1) ("b" . 2)))
(check (string->json "\t[\n1,\r\n2]\n") => #(1 2))
(check (string->json "   ") => (eof-object))

;;; [0125] 尾部垃圾与多顶层值均报 parse-error
(check-catch 'parse-error (string->json "[1,2] trailing"))
(check-catch 'parse-error (string->json "1 2"))

;;; 数字
(check (string->json "[0,-1,3.5,-0.25,1e2,1.5e-2]")
  =>
  #(0 -1 3.5 -0.25 100.0 0.015)
) ;check
(check (string->json "[123456789012345]") => #(123456789012345))

;;; 嵌套结构
(check (string->json "[[1,2],[3,4]]") => #(#(1 2) #(3 4)))
(check (string->json "{\"a\":{\"b\":{\"c\":1}}}") => '(("a" ("b" ("c" . 1)))))
(check (string->json "{\"k1\":{\"k2\":[1,{\"k3\":null}]}}")
  =>
  '(("k1" ("k2" . #(1 (("k3" . null))))))
) ;check
(check (string->json "[{},[]]") => #((()) #()))
(check (string->json "[[[]]]") => #(#(#())))

;;; 宽松语法
(check (string->json "{a-b:1}") => '((a-b . 1)))

;;; [0125] 非符号键报 parse-error：数字键、保留字键、'[' 开头键
(check-catch 'parse-error (string->json "{1:1}"))
(check-catch 'parse-error (string->json "{9999E9999:1}"))
(check-catch 'parse-error (string->json "{null:null,null:null}"))
(check-catch 'parse-error (string->json "{[: \"x\"}"))
(check-catch 'parse-error (string->json "[1,]"))
(check (string->json "[true,false,null]") => #(true false null))
(check-catch 'parse-error (string->json "{a:1,,}"))

;;; 字符串内容
(check (string->json "[\"\" ]") => #(""))
(check (string->json "[\"a,b:c{}[]\"]") => #("a,b:c{}[]"))
(check (string->json "[\"混合mixed文本\"]") => #("混合mixed文本"))
(check (string->json "[\"\\u004a\\u0061\"]") => #("Ja"))
(check (string->json "{\"\\u006b\":1}") => '(("k" . 1)))

;;; 截断与非法输入
(check-catch 'parse-error (string->json "{"))
(check-catch 'parse-error (string->json "{a:1"))
(check-catch 'parse-error (string->json "[1"))
(check-catch 'parse-error (string->json "\"abc"))
(check-catch 'parse-error (string->json "{a:}"))
(check-catch 'parse-error (string->json "[\"\\u0041"))

;;; 单引号字符串不是合法 JSON，应报 parse-error
(check-catch 'parse-error (string->json "{'key': 1}"))
(check-catch 'parse-error (string->json "['a','b']"))
(check-catch 'parse-error (string->json "'hello'"))
;;; 双引号字符串内的单引号是合法内容
(check (string->json "{\"a\": \"it's\"}") => '(("a" . "it's")))
(check (string->json "[\"don't\"]") => #("don't"))

(check-report)
