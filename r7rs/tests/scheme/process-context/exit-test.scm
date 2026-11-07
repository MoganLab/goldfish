;; exit
;; 运行 outstanding 的 dynamic-wind after 过程后终止当前进程。
;;
;; 语法
;; ----
;; (exit)
;; (exit obj)
;;
;; 参数
;; ----
;; obj : integer? 或 boolean? (可选)
;; 退出码。整数原样传递；#f 映射为 1，#t 或其他非整数值映射为 0。
;; 省略时按成功（0）退出。
;;
;; 返回值
;; ----
;; 不返回。调用后进程终止。
;;
;; 描述
;; ----
;; R7RS 6.14：exit 先运行所有活跃 dynamic-wind 的 after（outward）
;; 过程，再通过 libc exit 退出，允许 atexit 等清理函数执行。
;;
;; 错误处理
;; ----
;; 无。本测试通过子进程验证退出码，不在当前进程内直接调用
;; （否则会终止测试进程本身）。

(import (liii check)
  (liii os)
  (liii path)
  (liii sys)
  (scheme base)
  (scheme file)
  (scheme process-context)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? exit))

;; 以下用例仅在类 Unix 下运行
(when (or (os-linux?) (os-macos?))

  ;; 把代码写入临时脚本，由子进程执行，避免多层引号转义
  (define (write-script path forms)
    (call-with-output-file path
      (lambda (p)
        (display "(import (scheme base) (scheme process-context) (scheme file))" p)
        (newline p)
        (for-each
          (lambda (form)
            (write form p)
            (newline p)
          ) ;lambda
          forms
        ) ;for-each
      ) ;lambda
    ) ;call-with-output-file
  ) ;define

  (define (gf-run-script forms)
    (let ((script (path->string (path-join (os-temp-dir) "exit-test-script.scm"))))
      (write-script script forms)
      (let ((rc (os-call (string-append (executable) " " script))))
        (when (file-exists? script) (delete-file script))
        rc
      ) ;let
    ) ;let
  ) ;define

  ;; 整数参数原样传递
  (check (gf-run-script '((exit 5))) => 5)
  (check (gf-run-script '((exit 0))) => 0)
  (check (gf-run-script '((exit 42))) => 42)

  ;; 布尔与省略参数映射：#f -> 1，#t -> 0，省略 -> 0
  (check (gf-run-script '((exit #f))) => 1)
  (check (gf-run-script '((exit #t))) => 0)
  (check (gf-run-script '((exit))) => 0)

  ;; exit 运行 dynamic-wind 的 after：after 写标记文件，退出码仍为 5
  (let ((marker (path->string (path-join (os-temp-dir) "exit-dynamic-wind-marker"))))
    (when (file-exists? marker) (delete-file marker))
    (check
      (gf-run-script
        `((dynamic-wind (lambda () #f)
           (lambda () (exit 5))
           (lambda ()
             (with-output-to-file ,marker
               (lambda () (display "ran"))
             ) ;with-output-to-file
           ) ;lambda
         ) ;dynamic-wind
        ) ;list
      ) ;gf-run-script
      => 5
    ) ;check
    (check (file-exists? marker) => #t)
    (check (call-with-input-file marker (lambda (p) (read-line p))) => "ran")
    (delete-file marker)
  ) ;let

) ;when

(check-report)
