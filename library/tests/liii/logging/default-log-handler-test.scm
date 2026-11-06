;; default-log-handler
;; 按当前格式把一条日志消息写入指定端口。
;;
;; 语法
;; ----
;; (default-log-handler message port)
;;
;; 参数
;; ----
;; message : list
;; 日志 alist，含 SEVERITY 与 MESSAGE 字段。
;; port : port
;; 输出端口。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. 输出按 current-log-format 格式化，缺省含 23 位毫秒精度时间戳。
;; 2. 写入后立即 flush 输出端口。

(import (liii check) (liii logging) (liii string) (scheme write))

(check-set-mode! 'report-failed)

(define (char-digit? c)
  (char<=? #\0 c #\9)
) ;define

(define (valid-asctime? s)
  (and (string? s)
    (= (string-length s) 23)
    (char-digit? (string-ref s 0))
    (char-digit? (string-ref s 1))
    (char-digit? (string-ref s 2))
    (char-digit? (string-ref s 3))
    (char=? (string-ref s 4) #\-)
    (char-digit? (string-ref s 5))
    (char-digit? (string-ref s 6))
    (char=? (string-ref s 7) #\-)
    (char-digit? (string-ref s 8))
    (char-digit? (string-ref s 9))
    (char=? (string-ref s 10) #\space)
    (char-digit? (string-ref s 11))
    (char-digit? (string-ref s 12))
    (char=? (string-ref s 13) #\:)
    (char-digit? (string-ref s 14))
    (char-digit? (string-ref s 15))
    (char=? (string-ref s 16) #\:)
    (char-digit? (string-ref s 17))
    (char-digit? (string-ref s 18))
    (char=? (string-ref s 19) #\.)
    (char-digit? (string-ref s 20))
    (char-digit? (string-ref s 21))
    (char-digit? (string-ref s 22))
  ) ;and
) ;define

;; 基本输出：包含级别名与消息
(let ((p (open-output-string)))
  (default-log-handler '((SEVERITY . 6) (MESSAGE . "test")) p)
  (let ((output (get-output-string p)))
    (check (string-contains? output "test") => #t)
    (check (string-contains? output "INFO") => #t)
    ;; 缺省格式输出以 23 位毫秒精度时间戳开头
    (check (valid-asctime? (substring output 0 23)) => #t)
  ) ;let
) ;let

;; 毫秒精度补零对齐：连续输出的每行时间戳长度一致
(log-set-format! "%(asctime)s")
(let ((p (open-output-string)))
  (do ((i 0 (+ i 1)))
    ((= i 5))
    (default-log-handler '((SEVERITY . 6) (MESSAGE . "alignment check")) p)
  ) ;do
  (let ((lines (string-split (string-trim-right (get-output-string p)) #\newline)))
    (check (= (length lines) 5) => #t)
    (for-each
      (lambda (line)
        (check (valid-asctime? line) => #t)
        (check (= (string-length line) 23) => #t)
      ) ;lambda
      lines
    ) ;for-each
  ) ;let
) ;let

;; 恢复默认格式
(log-set-format! "%(asctime)s [%(levelname)s] %(message)s")

(check-report)
