(import (liii check) (liii json) (liii base) (liii error))

(check-set-mode! 'report)

;; read-json
;; 从输入端口读取并解析下一个 JSON 实体。
;;
;; 语法
;; ----
;; (read-json [port])
;;
;; 参数
;; ----
;; port : input-port?
;; 输入端口，可选，默认为 (current-input-port)。
;;
;; 返回值
;; ----
;; any?
;; 解析出的 Scheme-JSON 数据结构，或遇到流末尾时返回 eof-object。
;;
;; 注意
;; ----
;; 支持从端口连续流式读取多个 JSON 实体（如 JSONL/NDJSON 数据流）。
;;
;; 示例
;; ----
;; (read-json (open-input-string "{\"a\": 1}")) => '(("a" . 1))
;; (read-json (open-input-string "[1, 2, 3]")) => #(1 2 3)
;;
;; 错误处理
;; ----
;; type-error 当 port 不是输入端口时。
;; parse-error 当输入流中存在非法 JSON 格式时。

;; 1. 基本对象与数组读取
(check (read-json (open-input-string "{\"name\":\"goldfish\",\"version\":1}"))
  =>
  '(("name" . "goldfish") ("version" . 1))
) ;check

(check (read-json (open-input-string "[1, 2, 3]")) => #(1 2 3))
(check (read-json (open-input-string "{}")) => '(()))
(check (read-json (open-input-string "[]")) => #())

;; 2. 标量类型读取
(check (read-json (open-input-string "42")) => 42)
(check (read-json (open-input-string "-7")) => -7)
(check (read-json (open-input-string "3.14")) => 3.14)
(check (read-json (open-input-string "true")) => 'true)
(check (read-json (open-input-string "false")) => 'false)
(check (read-json (open-input-string "null")) => 'null)
(check (read-json (open-input-string "\"hello world\"")) => "hello world")

;; 3. 空白与 EOF 判定
(check (read-json (open-input-string "   [1, 2]   ")) => #(1 2))
(check (eof-object? (read-json (open-input-string ""))) => #t)
(check (eof-object? (read-json (open-input-string "   \t\n\r   "))) => #t)

;; 4. 流读取完毕后返回 EOF
(let ((p (open-input-string "{\"name\":\"goldfish\"}")))
  (check (read-json p) => '(("name" . "goldfish")))
  (check (eof-object? (read-json p)) => #t)
  (check (eof-object? (read-json p)) => #t)
) ;let

;; 5. 流式连续读取 JSON Lines (JSONL / NDJSON)
(let ((p (open-input-string "{\"line\":1}\n{\"line\":2}\n{\"line\":3}\n")))
  (check (read-json p) => '(("line" . 1)))
  (check (read-json p) => '(("line" . 2)))
  (check (read-json p) => '(("line" . 3)))
  (check (eof-object? (read-json p)) => #t)
) ;let

;; 6. 连续无空白多实体读取
(let ((p (open-input-string "{\"a\":1}{\"b\":2}")))
  (check (read-json p) => '(("a" . 1)))
  (check (read-json p) => '(("b" . 2)))
  (check (eof-object? (read-json p)) => #t)
) ;let

;; 7. 默认端口 (current-input-port)
(let ((orig (current-input-port)) (p (open-input-string "{\"default\":true}")))
  (set-current-input-port p)
  (let ((res (read-json)))
    (set-current-input-port orig)
    (check res => '(("default" . true)))
  ) ;let
) ;let

;; 8. 文件端口读取
(let ((tmp "read-json-temp.json"))
  (call-with-output-file tmp
    (lambda (p) (display "{\"name\":\"goldfish\",\"version\":1}" p))
  ) ;call-with-output-file
  (let ((res (call-with-input-file tmp (lambda (p) (read-json p)))))
    (delete-file tmp)
    (check (json-ref res "name") => "goldfish")
    (check (json-ref res "version") => 1)
  ) ;let
) ;let

;; 9. UTF-8 BOM 兼容与 Unicode
(let ((p (open-input-string "﻿{\"key\":\"中文😀\"}")))
  (check (read-json p) => '(("key" . "中文😀")))
) ;let

;; 10. 错误处理：非法端口类型抛出 type-error
(check-catch 'type-error (read-json "not-a-port"))
(check-catch 'type-error (read-json 123))
(check-catch 'type-error (read-json #f))

;; 11. 错误处理：语法错误抛出 parse-error
(check-catch 'parse-error (read-json (open-input-string "{")))
(check-catch 'parse-error (read-json (open-input-string "[1,")))
(check-catch 'parse-error (read-json (open-input-string "{\"a\":}")))
(check-catch 'parse-error (read-json (open-input-string "invalid")))

(check-report)
