;; read-raw-string-after-prefix
;; 在已读过 raw string 前缀（# 与起始引号）后继续读取内容。
;;
;; 语法
;; ----
;; (read-raw-string-after-prefix [port])
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
;; 该过程供 reader 钩子使用：调用时端口已停在起始引号处。
;; 输入形如 "<delimiter>"<content>"<delimiter>"。
;; 错误处理
;; ----
;; raw-string-read-error 当前缀不完整或内容未正常结束时抛出。

(import (liii check) (scheme file) (srfi srfi-267))


(check-set-mode! 'report-failed)


(check (read-raw-string-after-prefix (open-input-string "\"a\"\"")) => "a")

(check (read-raw-string-after-prefix (open-input-string "tag with space\"hello\"tag with space\""))
  =>
  "hello"
) ;check

(check (read-raw-string-after-prefix (open-input-string "(())\"#\"\"a\"\"\"(())\""))
  =>
  "#\"\"a\"\""
) ;check

;; 错误处理
(check-catch 'raw-string-read-error
  (read-raw-string-after-prefix (open-input-string "unterminated"))
) ;check-catch

(check-report)
