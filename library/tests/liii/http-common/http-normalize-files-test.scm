;; http-normalize-files
;; 规范化文件上传参数为统一的 part 描述关联列表。
;;
;; 语法
;; ----
;; (http-normalize-files who files)
;;
;; 参数
;; ----
;; who   : string?  调用方函数名称，用于错误信息前缀。
;; files : alist?   文件上传配置，值支持路径字符串或文件描述关联列表。
;;
;; 返回值
;; ----
;; alist?
;; 每项为 ((name . 名称) (file . 路径) (filename . 文件名)? (content-type . 类型)?)
;; 形式的规范化描述列表。
;;
;; 错误处理
;; ----
;; type-error
;; files 不是关联列表，或条目结构非法时抛出。
;; value-error
;; 文件路径不存在、描述缺少 file 键或含不支持的键时抛出。

(import (liii check) (liii http-common) (scheme file))

(check-set-mode! 'report-failed)

;; 在当前目录创建临时文件作为夹具，避免依赖测试运行目录下的资源文件
(define fixture "http-normalize-files-fixture.txt")
(call-with-output-file fixture (lambda (port) (display "fixture" port)))
(define string-input (list (cons "upload" fixture)))
(define string-expected (list (list (cons 'name "upload") (cons 'file fixture))))
(define full-input (list (cons "upload"
                      (list (cons 'file fixture)
                        (cons 'filename "fixture.txt")
                        (cons 'content-type "text/plain"))
                   ) ;cons
               ) ;cons
        ) ;full-input
(define full-expected
  (list (list (cons 'name "upload") (cons 'file fixture)
          (cons 'filename "fixture.txt") (cons 'content-type "text/plain"))
  ) ;list
) ;full-expected
(define symbol-input (list (cons 'upload fixture)))

;; 空列表返回空列表
(check (http-normalize-files "test" '()) => '())

;; 路径字符串形式的文件描述
(check-true (equal? (http-normalize-files "test" string-input) string-expected))

;; 完整文件描述关联列表
(check-true (equal? (http-normalize-files "test" full-input) full-expected))

;; 符号键转为字符串
(check-true (equal? (http-normalize-files "test" symbol-input) string-expected))

;; 非关联列表抛出 type-error
(check-catch 'type-error (http-normalize-files "test" "not-alist"))
(check-catch 'type-error (http-normalize-files "test" (list "a" "b")))

;; 值既非路径字符串也非描述列表抛出 type-error
(check-catch 'type-error (http-normalize-files "test" '(("upload" . 42))))

;; 文件不存在抛出 value-error
(check-catch 'value-error (http-normalize-files "test" '(("upload" . "/no/such/file.txt"))))

;; 描述缺少 file 键抛出 value-error
(check-catch 'value-error (http-normalize-files "test" '(("upload" . (("filename" . "a.txt"))))))

;; 描述含不支持的键抛出 value-error
(check-catch 'value-error
  (http-normalize-files "test"
    (list (cons "upload" (list (cons 'file fixture) (cons 'charset "utf-8"))))
  ) ;http-normalize-files
) ;check

;; 描述的值非字符串抛出 type-error
(check-catch 'type-error
  (http-normalize-files "test" (list (cons "upload" (list (cons 'file 42)))))
) ;check

(delete-file fixture)

(check-report)
