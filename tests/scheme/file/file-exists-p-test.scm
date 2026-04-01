(import (liii check)
        (scheme file)
) ;import

(check-set-mode! 'report-failed)

(define test-filename "file-test-exists.txt")

(define (clean-filename filename)
  (lambda () (when (file-exists? filename) (delete-file filename)))
) ;define

;; file-exists?
;; 检查文件是否存在。
;;
;; 语法
;; ----
;; (file-exists? filename)
;;
;; 参数
;; ----
;; filename : string
;;   要检查的文件名
;;
;; 返回值
;; ----
;; bool
;;   文件存在返回 #t，否则返回 #f
;;
;; 描述
;; ----
;; 检查指定路径的文件是否存在。
;; 注意：文件名编码问题可能导致返回 #f，即使文件存在。

; 测试不存在的文件
(check (file-exists? test-filename) => #f)
(check (file-exists? "non-existent-file.txt") => #f)
(check (file-exists? "/path/to/nonexistent/file") => #f)

; 测试创建后的文件存在
(dynamic-wind
  (lambda () ; before - do nothing
    #f
  ) ;lambda
  (lambda ()
    ; 创建文件前不存在
    (check (file-exists? test-filename) => #f)

    ; 创建文件
    (with-output-to-file test-filename
      (lambda () (display "test content"))
    ) ;with-output-to-file

    ; 创建文件后存在
    (check (file-exists? test-filename) => #t)
  ) ;lambda
  (lambda () ; after - cleanup
    (when (file-exists? test-filename)
      (delete-file test-filename)
    ) ;when
  ) ;lambda
) ;dynamic-wind

; 测试多语言文件名
(let ((unicode-filename "测试文件.txt"))
  (dynamic-wind
    (lambda () #f) ; before
    (lambda ()
      ; 文件不存在
      (check (file-exists? unicode-filename) => #f)

      ; 创建文件
      (with-output-to-file unicode-filename
        (lambda () (display "unicode test"))
      ) ;with-output-to-file

      ; 文件存在
      (check (file-exists? unicode-filename) => #t)
    ) ;lambda
    (lambda () ; after - cleanup
      (when (file-exists? unicode-filename)
        (delete-file unicode-filename)
      ) ;when
    ) ;lambda
  ) ;dynamic-wind
) ;let

(check-report)
