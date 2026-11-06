;; read-tzif-file
;; 读取 TZif 文件并解析为时区转移数据。
;;
;; 语法
;; ----
;; (read-tzif-file path-str)
;;
;; 参数
;; ----
;; path-str : string? TZif 文件路径。
;;
;; 返回值
;; ----
;; values 首个转移（tzif-transition? 或 #f）、转移向量（vector?）与 POSIX 规则（tz-string? 或 #f）。
;;
;; 错误处理
;; --------
;; type-error 当文件内容魔数非法时抛出错误。

(import (liii check)
  (liii os)
  (liii path)
  (liii tempfile)
  (liii date-time internal tzif)
) ;import

(check-set-mode! 'report-failed)

(define (u8-list->bv l)
  (let ((bv (make-bytevector (length l) 0)))
    (let loop
      ((i 0) (rem l))
      (if (null? rem)
        bv
        (begin
          (bytevector-u8-set! bv i (car rem))
          (loop (+ i 1) (cdr rem))
        ) ;begin
      ) ;if
    ) ;let
  ) ;let
) ;define

(define tzif-bv
  (u8-list->bv
    (append (map char->integer (string->list "TZif"))
      (make-list 16 0)
      (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 4)
      (list 0 0 0 0)
      (list 0)
      (list 0 0 14 16 1 0)
      (append (map char->integer (string->list "EST")) (list 0))
    ) ;append
  ) ;u8-list->bv
) ;define

(let-values (((port tmp-path) (make-temp-file "gf-tzif-" ".bin")))
  (close-port port)
  (path-write-bytes (path tmp-path) tzif-bv)
  (call-with-values (lambda () (read-tzif-file tmp-path))
    (lambda (first transitions footer)
      (check (tzif-transition-offset first) => 3600)
      (check (tzif-transition-designation first) => "EST")
      (check (vector-length transitions) => 1)
    ) ;lambda
  ) ;call-with-values
  (remove tmp-path)
) ;let-values

(check-report)
