(import (liii check)
        (scheme file)
) ;import

(check-set-mode! 'report-failed)

(define test-filename "file-test-load.scm")
(define test-filenames
  '("中文.scm"
    "測試.scm"
    "日本語.scm"
    "한글.scm"
    " ελληνικά.scm"
    "ملف.scm")
) ;define

(define (clean-filename filename)
  (lambda () (when (file-exists? filename) (delete-file filename)))
) ;define
(define clean-test-filename (clean-filename test-filename))

;; load
;; 加载并执行 Scheme 文件。
;;
;; 语法
;; ----
;; (load filename)
;;
;; 参数
;; ----
;; filename : string
;;   要加载的 Scheme 文件名
;;
;; 返回值
;; ----
;; any
;;   文件中最后一个表达式的求值结果
;;
;; 描述
;; ----
;; 读取并执行指定文件中的 Scheme 代码。
;; 文件中的定义会影响当前环境。

; 测试加载简单表达式
(dynamic-wind
  (lambda () ; before
    (with-output-to-file test-filename
      (lambda () (display "(+ 21 21)"))
    ) ;with-output-to-file
  ) ;lambda
  (lambda ()
    (check (load test-filename) => 42)
  ) ;lambda
  clean-test-filename ; after
) ;dynamic-wind

; 测试加载变量定义
(dynamic-wind
  (lambda () ; before
    (with-output-to-file test-filename
      (lambda ()
        (display "(define x 100)")
        (newline)
        (display "(+ x 1)")
      ) ;lambda
    ) ;with-output-to-file
  ) ;lambda
  (lambda ()
    (check (load test-filename) => 101)
  ) ;lambda
  clean-test-filename ; after
) ;dynamic-wind

; 测试加载函数定义
(dynamic-wind
  (lambda () ; before
    (with-output-to-file test-filename
      (lambda ()
        (display "(define (square x) (* x x))")
        (newline)
        (display "(square 5)")
      ) ;lambda
    ) ;with-output-to-file
  ) ;lambda
  (lambda ()
    (check (load test-filename) => 25)
  ) ;lambda
  clean-test-filename ; after
) ;dynamic-wind

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
          (lambda () (display "(+ 21 21)"))
        ) ;with-output-to-file

        ; 验证能够正常 load
        (check (load filename) => 42)
      ) ;lambda
      (clean-filename filename) ; after
    ) ;dynamic-wind
  ) ;lambda
  test-filenames
) ;for-each

(check-report)
