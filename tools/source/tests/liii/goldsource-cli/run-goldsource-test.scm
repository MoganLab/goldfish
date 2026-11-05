;; run-goldsource
;; 执行 gf source 命令行主流程，输出帮助或打印指定库源码。
;; 可显式传入参数列表供单元测试驱动；未给出时从 (argv) 解析。
;;
;; 语法
;; ----
;; (run-goldsource)
;; (run-goldsource args)
;;
;; 参数
;; ----
;; args : list?
;; 可选的参数字符串列表。
;;
;; 返回值
;; ------
;; integer?
;; 退出状态码：0 表示成功，1 表示失败或参数无效。

(import (scheme base)
  (liii base)
  (liii check)
  (liii string)
  (liii goldsource-cli)
) ;import

(check-set-mode! 'report-failed)

;; 测试：--help 打印用法并返回 0
(let ((out "") (ret #f))
  (set! out (with-output-to-string
              (lambda ()
                (set! ret (run-goldsource '("--help")))
              ) ;lambda
            ) ;with-output-to-string
  ) ;set!
  (check ret => 0)
  (check-true (string-contains? out "Usage:"))
  (check-true (string-contains? out "gf source ORG/LIB"))
) ;let

;; 测试：查询已存在的库打印其源代码并返回 0
(let ((out "") (ret #f))
  (set! out (with-output-to-string
              (lambda ()
                (set! ret (run-goldsource '("liii/string")))
              ) ;lambda
            ) ;with-output-to-string
  ) ;set!
  (check ret => 0)
  (check-true (string-contains? out "(define-library (liii string)"))
) ;let

;; 测试：查询不存在的库向标准错误报错并返回 1
(let ((err (open-output-string))
      (orig-err (current-error-port))
      (ret #f)
     ) ;
  (set-current-error-port err)
  (set! ret (run-goldsource '("liii/missing-library-not-found")))
  (set-current-error-port orig-err)
  (check ret => 1)
  (check-true (string-contains? (get-output-string err)
                "Error: library not found in *load-path*: liii/missing-library-not-found"
              ) ;string-contains?
  ) ;check-true
) ;let

;; 测试：无参数或多余参数打印用法并返回 1
(let ((out "") (ret #f))
  (set! out (with-output-to-string
              (lambda ()
                (set! ret (run-goldsource '("invalid" "extra" "args")))
              ) ;lambda
            ) ;with-output-to-string
  ) ;set!
  (check ret => 1)
  (check-true (string-contains? out "Usage:"))
) ;let

(check-report)
