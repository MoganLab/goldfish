;; catch-error->string
;; 综合格式化 catch 捕获到的异常信息，包含消息展开、位置与提示。
;;
;; 语法
;; ----
;; (catch-error->string tag info) -> string
;;
;; 参数
;; ----
;; tag : symbol or any
;; catch 捕获到的错误标签。
;;
;; info : list or any
;; catch 传递给错误处理器的参数。
;;
;; 返回值
;; ----
;; string
;; 格式化后的完整错误提示字符串。

(import (liii check)
  (liii string)
  (liii error)
  (liii os)
  (liii path)
) ;import

(check-set-mode! 'report-failed)

;; 1. 普通纯字符串错误
(check (catch-error->string 'user-error '("bad argument")) => "bad argument")
(check (catch-error->string 'user-error "bad argument direct") => "bad argument direct")

;; 2. 带占位符的嵌套格式化错误展开
(let ((res (catch-error->string 'type-error '(("expected ~S but got ~S" integer "abc")))))
  (check res => "expected integer but got \"abc\"")
) ;let

;; 3. 三层嵌套结构的展开（lambda (tag . info) 场景）
(let ((res (catch-error->string 'type-error '((("expected ~S but got ~S" integer "abc"))))))
  (check res => "expected integer but got \"abc\"")
) ;let

;; 4. 未绑定变量错误包含展开的符号名与代码片段
(let ((res (catch-error->string 'unbound-variable
             '(("unbound variable ~S in ~S" unknown-symbol (+ unknown-symbol 1)))
           ) ;catch-error->string
      ) ;res
     ) ;
  (check-true (string-contains? res "unbound variable unknown-symbol in (+ unknown-symbol 1)"))
) ;let

;; 5. 端到端真实异常测试：未绑定变量自动输出错误信息、代码位置与 import Hint
(let ((res (catch #t
             (lambda () (eval '(read-json "{}") (rootlet)))
             (lambda (tag . info) (catch-error->string tag info))
           ) ;catch
      ) ;res
     ) ;
  (check-true (string? res))
  (check-true (string-contains? res "unbound variable read-json"))
  (check-true (string-contains? res "Hint: `read-json` exists in library:"))
  (check-true (string-contains? res "(import (liii json))"))
) ;let

;; 6. 从真实文件加载代码时，未绑定变量必须定位到该文件及实际行号。
;; 文件输入端口的 file-number 若错误保持为 0，这里会显示 *stdout* 或其他库。
(let* ((source-path (path-join (path-temp-dir)
                      (string-append "gf-error-location-" (number->string (getpid)) ".scm")
                    ) ;path-join
       ) ;source-path
       (source-file (path->string source-path))
       (location (string-append source-file ":2"))
      ) ;
  (dynamic-wind (lambda () #f)
    (lambda ()
      (path-write-text source-path "\n(gf-error-location-unbound)\n")
      (let ((result (catch #t
                      (lambda () (load source-file))
                      (lambda (tag . info) (catch-error->string tag info))
                    ) ;catch
            ) ;result
           ) ;
        (check-true (string-contains? result "unbound variable gf-error-location-unbound"))
        (check-true (string-contains? result location))
      ) ;let
    ) ;lambda
    (lambda () (path-unlink source-path #t))
  ) ;dynamic-wind
) ;let*

;; 7. 先加载多个库，再加载一个有未绑定变量的库，位置不得串到其他库。
(let* ((source-path (path-join (path-temp-dir)
                      (string-append "gf-library-error-location-" (number->string (getpid)) ".scm")
                    ) ;path-join
       ) ;source-path
       (source-file (path->string source-path))
       (location (string-append source-file ":8"))
      ) ;
  (dynamic-wind (lambda () #f)
    (lambda ()
      (path-write-text source-path
        (string-append "(define-library (fixture error-location)\n"
          "  (import (liii trie) (liii sort) (liii uuid) (liii logging)\n"
          "    (liii flexvector) (liii iset) (liii random))\n"
          "  (export trigger-error-location)\n"
          "  (begin\n"
          "    (define (trigger-error-location)\n"
          "      #t\n"
          "      (gf-library-error-location-unbound)))\n"
          ")\n"
        ) ;string-append
      ) ;path-write-text
      (let ((result (catch #t
                      (lambda ()
                        (load source-file)
                        (eval '(import (fixture error-location)) (rootlet))
                        (eval '(trigger-error-location) (rootlet))
                      ) ;lambda
                      (lambda (tag . info) (catch-error->string tag info))
                    ) ;catch
            ) ;result
           ) ;
        (check-true (string-contains? result "unbound variable gf-library-error-location-unbound"))
        (check-true (string-contains? result location))
      ) ;let
    ) ;lambda
    (lambda () (path-unlink source-path #t))
  ) ;dynamic-wind
) ;let*

;; 8. 文件编号超过 255 且文件位置非零时，位置打包不得污染文件编号。
;; 历史上 run.scm 的编号 353 会被解码为其他文件的 97，
;; 因而把 filter 错误误报为 truncate.scm/vstack.scm。
(let* ((source-path (path-join (path-temp-dir)
                      (string-append "gf-packed-location-" (number->string (getpid)) ".scm")
                    ) ;path-join
       ) ;source-path
       (other-path (path-join (path-temp-dir)
                     (string-append "gf-packed-location-other-" (number->string (getpid)) ".scm")
                   ) ;path-join
       ) ;other-path
       (source-file (path->string source-path))
       (location (string-append source-file ":1"))
      ) ;
  (dynamic-wind (lambda () #f)
    (lambda ()
      (path-write-text source-path "(gf-packed-location-unbound)\n")
      (let loop ((i 0))
        (when (< i 300)
          (let ((file (string-append (path->string other-path) "." (number->string i))))
            (path-write-text file "()")
            (close-input-port (open-input-file file))
            (delete-file file)
          ) ;let
          (loop (+ i 1))
        ) ;when
      ) ;let
      (let ((result (catch #t
                      (lambda () (load source-file))
                      (lambda (tag . info) (catch-error->string tag info))
                    ) ;catch
            ) ;result
           ) ;
        (check-true (string-contains? result "unbound variable gf-packed-location-unbound"))
        (check-true (string-contains? result location))
      ) ;let
    ) ;lambda
    (lambda ()
      (path-unlink source-path #t)
      (path-unlink other-path #t)
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report)
