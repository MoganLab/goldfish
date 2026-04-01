;; Tests for scheme file with-output-to-file function

(import (liii check)
        (scheme file)
) ;import

(check-set-mode! 'report-failed)

(define test-filename "file-test-中文.txt")
(define test-filenames
  '("中文.txt"
    "測試.txt"
    "日本語.txt"
    "한글.txt"
    " ελληνικά.txt"
    "ملف.txt")
) ;define

(define (clean-filename filename)
  (lambda () (delete-file filename))
) ;define
(define clean-test-filename (clean-filename test-filename))

;; with-output-to-file
;; 将输出重定向到文件。
;;
;; 语法
;; ----
;; (with-output-to-file filename thunk)
;;
;; 参数
;; ----
;; filename : string
;;   目标文件名
;; thunk : procedure
;;   无参数的过程，其输出将被重定向到文件
;;
;; 返回值
;; ----
;; undefined
;;
;; 描述
;; ----
;; 打开文件用于输出，将当前输出端口重定向到该文件，
;; 执行 thunk，然后恢复原始输出端口。

; 中文文件名，中文内容
(check
  (dynamic-wind
    #f ; before
    (lambda ()
      (with-output-to-file test-filename
        (lambda () (display "测试内容"))
      ) ;with-output-to-file

      (call-with-input-file test-filename
        (lambda (port)
          (read-line port)
        ) ;lambda
      ) ;call-with-input-file
    ) ;lambda
    clean-test-filename ; after
  ) ;dynamic-wind
  => "测试内容"
) ;check

; 中文文件名，英文内容
(check
  (dynamic-wind
    #f ; before
    (lambda ()
      (with-output-to-file test-filename
        (lambda () (display "ok"))
      ) ;with-output-to-file

      (call-with-input-file test-filename
        (lambda (port)
          (read-line port)
        ) ;lambda
      ) ;call-with-input-file
    ) ;lambda
    clean-test-filename ; after
  ) ;dynamic-wind
  => "ok"
) ;check

; 中文文件名，多行中文内容
(check
  (dynamic-wind
    #f ; before
    (lambda ()
      (with-output-to-file test-filename
        (lambda ()
          (display "第一行\n")
          (display "第二行")
        ) ;lambda
      ) ;with-output-to-file

      (call-with-input-file test-filename
        (lambda (port)
          (list (read-line port)
                (read-line port)
          ) ;list
        ) ;lambda
      ) ;call-with-input-file
    ) ;lambda
    clean-test-filename ; after
  ) ;dynamic-wind
  => '("第一行" "第二行")
) ;check

; 测试多种语言文件名
(for-each
  (lambda (filename)
    (dynamic-wind
      #f ; before
      (lambda ()
        ; 确保测试文件还不存在
        (check (file-exists? filename) => #f)

        ; 测试文件创建
        (with-output-to-file filename
          (lambda () (display "test"))
        ) ;with-output-to-file

        ; 验证文件存在
        (check-true (file-exists? filename))
      ) ;lambda
      (clean-filename filename) ; after
    ) ;dynamic-wind
  ) ;lambda
  test-filenames
) ;for-each

(check-report)
