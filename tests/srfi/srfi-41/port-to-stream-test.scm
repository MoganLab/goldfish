;; port->stream
;; 把输入端口当前及后续字符转换为字符流。
;;
;; 语法
;; ----
;; (port->stream [port])
;;
;; 参数
;; ----
;; port : input-port?
;; 输入端口，缺省为当前输入端口。
;; 返回值
;; ----
;; stream?
;; 端口剩余内容的字符流。
;;
;; 说明
;; ----
;; 错误：非输入端口报 non-input-port argument。
;; 读到 EOF 时流终止，端口不自动关闭。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'port->stream (port->stream "four"))
(let* ((p (open-input-string "abc"))
       (s (port->stream p)))
  (check (stream->list s) => '(#\a #\b #\c))
  (close-port p))

(check-report)
