(import (liii check)
        (scheme file)
) ;import

(check-set-mode! 'report-failed)

(define test-filename "file-test-delete.txt")

;; delete-file
;; 删除文件。
;;
;; 语法
;; ----
;; (delete-file filename)
;;
;; 参数
;; ----
;; filename : string
;;   要删除的文件名
;;
;; 返回值
;; ----
;; undefined
;;
;; 描述
;; ----
;; 删除指定路径的文件。如果文件不存在，行为取决于实现。
;; 通常在删除前应先检查文件是否存在。

; 测试创建并删除文件
(dynamic-wind
  (lambda () ; before
    ; 确保文件存在
    (with-output-to-file test-filename
      (lambda () (display "to be deleted"))
    ) ;with-output-to-file
  ) ;lambda
  (lambda ()
    ; 确认文件存在
    (check (file-exists? test-filename) => #t)

    ; 删除文件
    (delete-file test-filename)

    ; 确认文件已删除
    (check (file-exists? test-filename) => #f)
  ) ;lambda
  (lambda () ; after - ensure cleanup
    (when (file-exists? test-filename)
      (delete-file test-filename)
    ) ;when
  ) ;lambda
) ;dynamic-wind

; 测试删除多语言文件名文件
(let ((unicode-filename "删除测试.txt"))
  (dynamic-wind
    (lambda () ; before
      (with-output-to-file unicode-filename
        (lambda () (display "delete me"))
      ) ;with-output-to-file
    ) ;lambda
    (lambda ()
      ; 确认文件存在
      (check (file-exists? unicode-filename) => #t)

      ; 删除文件
      (delete-file unicode-filename)

      ; 确认文件已删除
      (check (file-exists? unicode-filename) => #f)
    ) ;lambda
    (lambda () ; after
      (when (file-exists? unicode-filename)
        (delete-file unicode-filename)
      ) ;when
    ) ;lambda
  ) ;dynamic-wind
) ;let

(check-report)
