(import (liii check)
  (liii json)
  (liii base)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

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

(check (string->json "{\"name\":\"Bob\",\"age\":21}"
       ) ;string->json
  =>
  '(("name" . "Bob") ("age" . 21))
) ;check
(check (string->json "[1,2,3]")
  =>
  #(1 2 3)
) ;check
(check (string->json "[]") => #())
(check (string->json "[true]")
  =>
  #(true)
) ;check
(check (string->json "[false]")
  =>
  #(false)
) ;check
(check (string->json "[{data: 1},{}]")
  =>
  #(((data . 1)) (()))
) ;check
(check (string->json "{}") => '(()))
(check (string->json "{args: {}}")
  =>
  '((args ()))
) ;check
(check (string->json "{\"args\": {}}")
  =>
  '(("args" ()))
) ;check
(check (string->json "{\"args\": {}, data: 1}")
  =>
  '(("args" ()) (data . 1))
) ;check
(check (string->json "{\"args\": {}, data: [1,2,3]}"
       ) ;string->json
  =>
  '(("args" ()) (data . #(1 2 3)))
) ;check
(check (string->json "{\"args\": {}, data: true}"
       ) ;string->json
  =>
  '(("args" ()) (data . true))
) ;check
(check (string->json "{\"args\": {}, data: null}"
       ) ;string->json
  =>
  '(("args" ()) (data . null))
) ;check
(check (string->json "{a:{b:1,c:2}}")
  =>
  '((a (b . 1) (c . 2)))
) ;check

(check (string->json "{\"age\":18}")
  =>
  '(("age" . 18))
) ;check
(check (string->json "{age:18}")
  =>
  '((age . 18))
) ;check
(check (string->json "{\"name\":\"中文\"}")
  =>
  '(("name" . "中文"))
) ;check
(check (string->json "{\"name\":\"Alice\\nBob\"}"
       ) ;string->json
  =>
  '(("name" . "Alice\nBob"))
) ;check
(check (string->json "{\"name\":\"Alice\\tBob\"}"
       ) ;string->json
  =>
  '(("name" . "Alice\tBob"))
) ;check
(check (string->json "{\"name\":\"Alice\\rBob\"}"
       ) ;string->json
  =>
  '(("name" . "Alice\rBob"))
) ;check
(check (string->json "{\"name\":\"Alice\\bBob\"}"
       ) ;string->json
  =>
  '(("name" . "Alice\bBob"))
) ;check
(check (string->json "{\"name\":\"Alice\\fBob\"}"
       ) ;string->json
  =>
  '(("name" . "Alice\fBob"))
) ;check
(check (string->json "{\"name\":\"Alice\\\\Bob\"}"
       ) ;string->json
  =>
  '(("name" . "Alice\\Bob"))
) ;check
(check (string->json "{\"name\":\"Alice\\/Bob\"}"
       ) ;string->json
  =>
  '(("name" . "Alice/Bob"))
) ;check
(check (string->json "{\"name\":\"Alice\\\"Bob\"}"
       ) ;string->json
  =>
  '(("name" . "Alice\"Bob"))
) ;check
(check (string->json "[\"\\u0041\"]")
  =>
  #("A")
) ;check
(check (string->json "[\"\\u0041\\u0042\"]")
  =>
  #("AB")
) ;check
(check (string->json "[\"\\u4E2D\\u6587\"]")
  =>
  #("中文")
) ;check
(check (string->json "[\"\\uD83D\\uDE00\"]")
  =>
  #("😀")
) ;check
(check (string->json "{\"name\":\"\\u4E2D\\u6587\"}"
       ) ;string->json
  =>
  '(("name" . "中文"))
) ;check
(check (string->json "{\"emoji\":\"\\uD83D\\uDE00\"}"
       ) ;string->json
  =>
  '(("emoji" . "😀"))
) ;check
(check-catch 'parse-error
  (string->json "[\"\\u004G\"]")
) ;check-catch
(check-catch 'parse-error
  (string->json "[\"\\a\"]")
) ;check-catch
(check (string->json "")
  =>
  (eof-object)
) ;check
(check (string->json ".")
  =>
  (eof-object)
) ;check
(check-catch 'read-error
  (string->json "[")
) ;check-catch

(check-report)
