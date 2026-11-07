;; read-raw-string
;; 从端口读取 raw string 字面量。
;;
;; 语法
;; ----
;; (read-raw-string [port])
;;
;; 参数
;; ----
;; port : input-port 可选
;; 读取来源端口，默认使用当前输入端口。
;; 返回值
;; ----
;; string
;; 返回读取到的 raw string 内容。
;; 说明
;; ----
;; 读取的语法形如 #"<delimiter>"<content>"<delimiter>"，
;; 内容中出现的 delimiter 必须是转义过的形式。
;; 错误处理
;; ----
;; raw-string-read-error 当输入不以 #" 开头、内容未正常结束或遇到意外的文件结尾时抛出。

(import (liii check) (scheme file) (srfi srfi-267))


(check-set-mode! 'report-failed)


(check (read-raw-string (open-input-string "#\"\"a\"\"")) => "a")

(check (read-raw-string (open-input-string "#\"tag with space\"hello\"tag with space\""))
  =>
  "hello"
) ;check

(check (read-raw-string (open-input-string "#\"(())\"#\"\"a\"\"\"(())\""))
  =>
  "#\"\"a\"\""
) ;check

;; 读取后端口停在结束引号之后
(let ((port (open-input-string "#\"\"\"\"\"\"")))
  (check (read-raw-string port) => "")
  (check (read-char port) => #\")
) ;let

;; 错误处理
(check-catch 'raw-string-read-error (read-raw-string (open-input-string "#x")))
(check-catch 'raw-string-read-error (read-raw-string (open-input-string "#\"unterminated")))

(check-report)
