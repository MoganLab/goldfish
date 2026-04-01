(import (liii check)
        (scheme file)
) ;import

(check-set-mode! 'report-failed)

(define test-filename "file-test-call.txt")

(define (clean-filename filename)
  (lambda () (when (file-exists? filename) (delete-file filename)))
) ;define
(define clean-test-filename (clean-filename test-filename))

;; call-with-input-file
;; 用输入文件调用函数。
;;
;; 语法
;; ----
;; (call-with-input-file filename proc)
;;
;; 参数
;; ----
;; filename : string
;;   输入文件名
;; proc : procedure
;;   接受一个输入端口作为参数的过程
;;
;; 返回值
;; ----
;; any
;;   proc 的返回值
;;
;; 描述
;; ----
;; 打开文件用于输入，将文件端口传递给 proc，
;; 在 proc 返回后自动关闭端口。

(dynamic-wind
  (lambda () ; before
    (with-output-to-file test-filename
      (lambda () (display "Hello, World!"))
    ) ;with-output-to-file
  ) ;lambda
  (lambda ()
    (check
      (call-with-input-file test-filename
        (lambda (port) (read-line port))
      ) ;call-with-input-file
      => "Hello, World!"
    ) ;check

    ; 测试多次读取
    (check
      (call-with-input-file test-filename
        (lambda (port)
          (list (read-char port)
                (read-char port)
                (read-char port)
          ) ;list
        ) ;lambda
      ) ;call-with-input-file
      => '(#\H #\e #\l)
    ) ;check
  ) ;lambda
  clean-test-filename ; after
) ;dynamic-wind

; 测试读取多行
(dynamic-wind
  (lambda () ; before
    (with-output-to-file test-filename
      (lambda ()
        (display "Line 1\n")
        (display "Line 2\n")
        (display "Line 3")
      ) ;lambda
    ) ;with-output-to-file
  ) ;lambda
  (lambda ()
    (check
      (call-with-input-file test-filename
        (lambda (port)
          (list (read-line port)
                (read-line port)
                (read-line port)
          ) ;list
        ) ;lambda
      ) ;call-with-input-file
      => '("Line 1" "Line 2" "Line 3")
    ) ;check
  ) ;lambda
  clean-test-filename ; after
) ;dynamic-wind

(check-report)
