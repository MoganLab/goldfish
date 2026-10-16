(import (liii check) (liii json) (liii base) (liii error))

(check-set-mode! 'report)

;; write-json
;; 将 Scheme-JSON 数据结构序列化写入指定输出端口。
;;
;; 语法
;; ----
;; (write-json json-value [port])
;;
;; 参数
;; ----
;; json-value : any?
;; 待序列化的 Scheme 对象。
;; port : output-port?
;; 输出端口，可选，默认为 (current-output-port)。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 注意
;; ----
;; 与 json->string 共享序列化规则。
;;
;; 示例
;; ----
;; (let ((p (open-output-string)))
;;   (write-json '(("a" . 1)) p)
;;   (get-output-string p)) => "{\"a\":1}"
;;
;; 错误处理
;; ----
;; type-error 当 port 不是输出端口或 json-value 包含不可序列化的过程对象时。
;; value-error 当输入不是合法的 JSON 数据结构时。

;; 1. 基本对象与数组写入字符串端口
(let ((p (open-output-string)))
  (write-json '(("name" . "Alice") ("age" . 20)) p)
  (check (get-output-string p) => "{\"name\":\"Alice\",\"age\":20}")
) ;let

(let ((p (open-output-string)))
  (write-json #(1 2 3) p)
  (check (get-output-string p) => "[1,2,3]")
) ;let

(let ((p (open-output-string)))
  (write-json '(()) p)
  (check (get-output-string p) => "{}")
) ;let

(let ((p (open-output-string)))
  (write-json #() p)
  (check (get-output-string p) => "[]")
) ;let

;; 2. 标量类型写入
(let ((p (open-output-string)))
  (write-json 42 p)
  (check (get-output-string p) => "42")
) ;let

(let ((p (open-output-string)))
  (write-json #t p)
  (check (get-output-string p) => "true")
) ;let

(let ((p (open-output-string)))
  (write-json 'null p)
  (check (get-output-string p) => "null")
) ;let

(let ((p (open-output-string)))
  (write-json "hello\nworld" p)
  (check (get-output-string p) => "\"hello\\nworld\"")
) ;let

;; 3. 读写回环 (Round-trip)
(let* ((data '(("status" . "ok") ("code" . 200))) (p (open-output-string)))
  (write-json data p)
  (let ((s (get-output-string p)))
    (check (read-json (open-input-string s)) => data)
  ) ;let
) ;let*

;; 4. 写入文件端口
(let ((tmp "write-json-temp.json") (data '(("status" . "ok") ("code" . 200))))
  (call-with-output-file tmp (lambda (p) (write-json data p)))
  (let ((res (call-with-input-file tmp (lambda (p) (read-json p)))))
    (delete-file tmp)
    (check (json-ref res "status") => "ok")
    (check (json-ref res "code") => 200)
  ) ;let
) ;let

;; 5. 默认端口 (current-output-port)
(let ((orig (current-output-port)) (p (open-output-string)))
  (set-current-output-port p)
  (write-json '(("hello" . "world")))
  (set-current-output-port orig)
  (check (get-output-string p) => "{\"hello\":\"world\"}")
) ;let

;; 6. 错误处理：非法端口类型抛出 type-error
(check-catch 'type-error (write-json '(()) "not-a-port"))
(check-catch 'type-error (write-json '(()) 123))
(check-catch 'type-error (write-json '(()) (open-input-string "")))

;; 7. 错误处理：非法数据类型（包含过程）抛出 type-error
(check-catch 'type-error
  (let ((p (open-output-string)))
    (write-json (lambda () 1) p)
  ) ;let
) ;check-catch

;; 8. 错误处理：非法 JSON 结构抛出 value-error
(check-catch 'value-error
  (let ((p (open-output-string)))
    (write-json '(("a")) p)
  ) ;let
) ;check-catch

(check-report)
