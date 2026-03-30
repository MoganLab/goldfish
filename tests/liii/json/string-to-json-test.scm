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

(check (string->json "{\"name\":\"Bob\",\"age\":21}") => `(("name" . "Bob") ("age" . 21)))
(check (string->json "[1,2,3]") => #(1 2 3))
(check (string->json "[]") => #())
(check (string->json "[true]") => #(true))
(check (string->json "[false]") => #(false))
(check (string->json "[{data: 1},{}]") => #(((data . 1)) (())))
(check (string->json "{}") => '(()))
(check (string->json "{args: {}}") => '((args ())))
(check (string->json "{\"args\": {}}") => '(("args" ())))
(check (string->json "{\"args\": {}, data: 1}") => '(("args" ()) (data . 1)))
(check (string->json "{\"args\": {}, data: [1,2,3]}") => '(("args" ()) (data . #(1 2 3))))
(check (string->json "{\"args\": {}, data: true}") => `(("args" ()) (data . true)))
(check (string->json "{\"args\": {}, data: null}") => `(("args" ()) (data . null)))
(check (string->json "{a:{b:1,c:2}}") => '((a . ((b . 1) (c . 2)))))

(check (string->json "{\"age\":18}") => `(("age" . 18)))
(check (string->json "{age:18}") => `((age . 18)))
(check (string->json "{\"name\":\"中文\"}") => `(("name" . "中文")))
(check (string->json "{\"name\":\"Alice\\nBob\"}") => '(("name" . "Alice\nBob")))
(check (string->json "{\"name\":\"Alice\\tBob\"}") => '(("name" . "Alice\tBob")))
(check (string->json "{\"name\":\"Alice\\rBob\"}") => '(("name" . "Alice\rBob")))
(check (string->json "{\"name\":\"Alice\\bBob\"}") => '(("name" . "Alice\bBob")))
(check (string->json "{\"name\":\"Alice\\fBob\"}") => '(("name" . "Alice\fBob")))
(check (string->json "{\"name\":\"Alice\\\\Bob\"}") => '(("name" . "Alice\\Bob")))
(check (string->json "{\"name\":\"Alice\\/Bob\"}") => '(("name" . "Alice/Bob")))
(check (string->json "{\"name\":\"Alice\\\"Bob\"}") => '(("name" . "Alice\"Bob")))
(check (string->json "[\"\\u0041\"]") => #("A"))
(check (string->json "[\"\\u0041\\u0042\"]") => #("AB"))
(check (string->json "[\"\\u4E2D\\u6587\"]") => #("中文"))
(check (string->json "[\"\\uD83D\\uDE00\"]") => #("😀"))
(check (string->json "{\"name\":\"\\u4E2D\\u6587\"}") => '(("name" . "中文")))
(check (string->json "{\"emoji\":\"\\uD83D\\uDE00\"}") => '(("emoji" . "😀")))
(check-catch 'parse-error (string->json "[\"\\u004G\"]"))
(check-catch 'parse-error (string->json "[\"\\a\"]"))
(check (string->json "") => (eof-object))
(check (string->json ".") => (eof-object))
(check-catch 'read-error (string->json "["))

(check-report)
