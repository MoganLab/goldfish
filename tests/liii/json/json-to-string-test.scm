(import (liii check)
        (liii json)
        (liii base)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json->string
;; 将 Scheme 形式的 JSON 数据编码为字符串。
;;
;; 语法
;; ----
;; (json->string data)
;;
;; 参数
;; ----
;; data : any?
;; 要编码的 JSON 数据结构。
;;
;; 返回值
;; ----
;; string
;; 返回对应的 JSON 字符串。
;;
;; 注意
;; ----
;; 对象键名在部分情况下会输出为宽松格式，不总是强制带引号。
;;
;; 示例
;; ----
;; (json->string #(1 2 3)) => "[1,2,3]"
;; (json->string '((a . ((b . 1) (c . 2))))) => "{a:{b:1,c:2}}"
;;
;; 错误处理
;; ----
;; value-error 当输入不是有效的 JSON 数据结构时。

(check (json->string #(1 2 3)) => "[1,2,3]")
(check (json->string '((a . ((b . 1) (c . 2))))) => "{a:{b:1,c:2}}")
(check-catch 'value-error (json->string '((a))))
(check (json->string #()) => "[]")
(check (json->string #("a" "b")) => "[\"a\",\"b\"]")
(check (json->string #(1 "a" true null)) => "[1,\"a\",true,null]")
(check (json->string '(("name" . "Alice"))) => "{\"name\":\"Alice\"}")
(check (json->string '(("id" . 1) ("active" . true))) => "{\"id\":1,\"active\":true}")
(check (json->string '((name . "Bob"))) => "{name:\"Bob\"}")
(check (json->string '((x . 10) (y . 20))) => "{x:10,y:20}")
(check (json->string #((("id" . 1)) (("id" . 2)))) => "[{\"id\":1},{\"id\":2}]")
(check (json->string '(("scores" . #(85 90 95)))) => "{\"scores\":[85,90,95]}")
(check (json->string '(("user" . (("name" . "Dave")
                                   ("tags" . #("admin" "editor"))))))
       => "{\"user\":{\"name\":\"Dave\",\"tags\":[\"admin\",\"editor\"]}}"
) ;check
(check (json->string '(("text" . "Line1\nLine2"))) => "{\"text\":\"Line1\\nLine2\"}")
(check (json->string #("He said \"Hello\"")) => "[\"He said \\\"Hello\\\"\"]")

(check
  (json->string
    `(("messages" . #((("role" . "user") ("content" . #(1 2 3)))
                      (("role" . "user") ("content" . "中文")))))
  ) ;json->string
  => "{\"messages\":[{\"role\":\"user\",\"content\":[1,2,3]},{\"role\":\"user\",\"content\":\"中文\"}]}"
) ;check

(check
  (json->string
    `(("messages" . #(
        (("role" . "user") ("content" . #(
          (("text" . "1") ("type" . "text"))
          (("text" . "2") ("type" . "text"))
        )))
        (("role" . "user") ("content" . "中文"))
      )))
  ) ;json->string
  => "{\"messages\":[{\"role\":\"user\",\"content\":[{\"text\":\"1\",\"type\":\"text\"},{\"text\":\"2\",\"type\":\"text\"}]},{\"role\":\"user\",\"content\":\"中文\"}]}"
) ;check

(define sample-j
  '((user . ((id . 1001)
             (name . "Alice")
             (active . #t)
             (email . null)
             (tags . #("dev" "scheme" "json"))
             (profile . ((age . 21)
                         (height . 168.5)
                         (hobbies . #("music" "reading"))))))
    (scores . #(98 87 93))) ;define
) ;define

(check (json->string sample-j)
  => "{user:{id:1001,name:\"Alice\",active:true,email:null,tags:[\"dev\",\"scheme\",\"json\"],profile:{age:21,height:168.5,hobbies:[\"music\",\"reading\"]}},scores:[98,87,93]}"
) ;check

(check-report)
