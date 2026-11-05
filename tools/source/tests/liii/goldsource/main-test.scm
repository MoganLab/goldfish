;; main
;; gf source 子命令入口：解析命令行参数并显示指定库的源代码。
;; 可显式传入参数列表供单元测试驱动；未给出时从进程命令行 (argv) 解析。
;;
;; 语法
;; ----
;; (main)
;; (main args)
;;
;; 参数
;; ----
;; args : list?
;; 可选的参数字符串列表。未给出时从 (argv) 解析。
;;
;; 返回值
;; ------
;; integer?
;; 退出状态码：0 表示成功，1 表示失败或参数无效。

(import (scheme base)
  (liii base)
  (liii check)
  (liii os)
  (liii path)
  (liii string)
  (liii sys)
  (liii goldsource)
) ;import

(check-set-mode! 'report-failed)

;; 测试：--help 选项输出帮助并返回 0
(let ((out "") (ret #f))
  (set! out (with-output-to-string
              (lambda ()
                (set! ret (main '("--help")))
              ) ;lambda
            ) ;with-output-to-string
  ) ;set!
  (check ret => 0)
  (check-true (string-contains? out "Usage:"))
  (check-true (string-contains? out "gf source ORG/LIB"))
) ;let

;; 测试：直接查询现有可见库源代码并返回 0
(let ((out "") (ret #f))
  (set! out (with-output-to-string
              (lambda ()
                (set! ret (main '("liii/string")))
              ) ;lambda
            ) ;with-output-to-string
  ) ;set!
  (check ret => 0)
  (check-true (string-contains? out "(define-library (liii string)"))
) ;let

;; 测试：查询不存在的库向标准错误输出错误并返回 1
(let ((err (open-output-string))
      (orig-err (current-error-port))
      (ret #f)
     ) ;
  (set-current-error-port err)
  (set! ret (main '("liii/non-existent-library")))
  (set-current-error-port orig-err)
  (check ret => 1)
  (check-true (string-contains? (get-output-string err)
                "Error: library not found in *load-path*: liii/non-existent-library"
              ) ;string-contains?
  ) ;check-true
) ;let

;; 测试：通过自定义临时 load-path 查询隔离的库源码
(let* ((base-root (path-join (path-temp-dir)
                    (string-append "goldsource-main-" (number->string (getpid)))
                  ) ;path-join
       ) ;base-root
       (load-root (path-join base-root "goldfish"))
       (liii-root (path-join load-root "liii"))
       (demo-source (path-join liii-root "demo.scm"))
       (expected "(define-library (liii demo)\n  (export demo-value)\n  (import (scheme base))\n  (begin\n    (define demo-value 42)\n  ) ;begin\n) ;define-library\n")
       (orig-load-path *load-path*)
      ) ;
  (mkdir (path->string base-root))
  (mkdir (path->string load-root))
  (mkdir (path->string liii-root))
  (path-write-text demo-source expected)
  (dynamic-wind
    (lambda ()
      (set! *load-path* (cons (path->string load-root) *load-path*))
    ) ;lambda
    (lambda ()
      (let ((out "") (ret #f))
        (set! out (with-output-to-string
                    (lambda ()
                      (set! ret (main '("liii/demo")))
                    ) ;lambda
                  ) ;with-output-to-string
        ) ;set!
        (check ret => 0)
        (check out => expected)
      ) ;let
    ) ;lambda
    (lambda ()
      (set! *load-path* orig-load-path)
      (path-unlink demo-source #t)
      (path-rmdir liii-root)
      (path-rmdir load-root)
      (path-rmdir base-root)
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report)
