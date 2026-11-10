;; emergency-exit
;; 立即终止当前进程，不运行任何清理过程。
;;
;; 语法
;; ----
;; (emergency-exit)
;; (emergency-exit obj)
;;
;; 参数
;; ----
;; obj : integer? 或 boolean? (可选)
;; 退出码。整数原样传递；#f 映射为 1，#t 或其他非整数值映射为 0。
;; 省略时按成功（0）退出。
;;
;; 返回值
;; ----
;; 不返回。调用后进程立即终止。
;;
;; 描述
;; ----
;; R7RS 6.14：emergency-exit 直接调用 _exit，不运行 dynamic-wind 的
;; after 过程，也不执行 atexit 等清理函数；仅在需要绕过清理时使用
;; （例如退出处理过程本身出错时）。
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

(check-true (procedure? emergency-exit))

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
    (let ((script (path->string (path-join (os-temp-dir) "emergency-exit-test-script.scm"))))
      (write-script script forms)
      (let ((rc (os-call (string-append (executable) " " script))))
        (when (file-exists? script) (delete-file script))
        rc
      ) ;let
    ) ;let
  ) ;define

  ;; 整数参数原样传递
  (check (gf-run-script '((emergency-exit 7))) => 7)
  (check (gf-run-script '((emergency-exit 0))) => 0)

  ;; 布尔与省略参数映射：#f -> 1，#t -> 0，省略 -> 0
  (check (gf-run-script '((emergency-exit #f))) => 1)
  (check (gf-run-script '((emergency-exit #t))) => 0)
  (check (gf-run-script (list (quote (emergency-exit)))) => 0)

  ;; emergency-exit 不运行 dynamic-wind 的 after：标记文件不应出现
  (let ((marker (path->string (path-join (os-temp-dir) "emergency-exit-marker"))))
    (when (file-exists? marker) (delete-file marker))
    (check
      (gf-run-script
        `((dynamic-wind (lambda () #f)
           (lambda () (emergency-exit 6))
           (lambda ()
             (with-output-to-file ,marker
               (lambda () (display "ran"))
             ) ;with-output-to-file
           ) ;lambda
         ) ;dynamic-wind
        ) ;list
      ) ;gf-run-script
      => 6
    ) ;check
    (check (file-exists? marker) => #f)
  ) ;let

) ;when

(check-report)
