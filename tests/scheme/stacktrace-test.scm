(import (liii check) (liii os) (liii path) (liii string) (liii sys))

(check-set-mode! 'report-failed)

(define (run-shell-command command)
  (os-call (string-append "sh -c \"" command "\""))
) ;define

(when (not (os-windows?))
  ;; 1. 单文件简单调用栈测试
  (let* ((demo-file (path "demo/stacktrace/simple.scm"))
         (output-path
           (path-join (path-temp-dir)
             (string-append "gf-stacktrace-simple-test-" (number->string (getpid)) ".log")
           ) ;path-join
         ) ;output-path
        ) ;
    (path-unlink output-path #t)
    (dynamic-wind (lambda () #t)
      (lambda ()
        (check-true (path-exists? demo-file))
        (let ((cmd (string-append (executable)
                     " "
                     (path->string demo-file)
                     " > "
                     (path->string output-path)
                     " 2>&1"
                   ) ;string-append
              ) ;cmd
             ) ;
          (check-true (not (= (run-shell-command cmd) 0)))
        ) ;let
        (let ((output (path-read-text output-path)))
          (check-true (string-contains? output "Stack trace:"))
          (check-true (string-contains? output "demo/stacktrace/simple.scm:3:"))
          (check-true (string-contains? output "step-c"))
          (check-true (string-contains? output "demo/stacktrace/simple.scm:7:"))
          (check-true (string-contains? output "step-b"))
          (check-true (string-contains? output "demo/stacktrace/simple.scm:12:"))
          (check-true (string-contains? output "step-a"))
          (check-false (string-contains? output "gfproject.scm"))
        ) ;let
      ) ;lambda
      (lambda () (path-unlink output-path #t))
    ) ;dynamic-wind
  ) ;let*

  ;; 2. 标准库 (liii string) 输入类型检查报错调用栈测试
  (let* ((string-demo-file (path "demo/stacktrace/liii_string.scm"))
         (output-path
           (path-join (path-temp-dir)
             (string-append "gf-stacktrace-string-test-" (number->string (getpid)) ".log")
           ) ;path-join
         ) ;output-path
        ) ;
    (path-unlink output-path #t)
    (dynamic-wind (lambda () #t)
      (lambda ()
        (check-true (path-exists? string-demo-file))
        (let ((cmd (string-append (executable)
                     " "
                     (path->string string-demo-file)
                     " > "
                     (path->string output-path)
                     " 2>&1"
                   ) ;string-append
              ) ;cmd
             ) ;
          (check-true (not (= (run-shell-command cmd) 0)))
        ) ;let
        (let ((output (path-read-text output-path)))
          (check-true (string-contains? output "Stack trace:"))
          (check-true (string-contains? output "goldfish/liii/string.scm:44:"))
          (check-true (string-contains? output "string-remove-suffix"))
          (check-true (string-contains? output "demo/stacktrace/liii_string.scm:8:"))
          (check-true (string-contains? output "clean-file-extension"))
          (check-true (string-contains? output "demo/stacktrace/liii_string.scm:14:"))
          (check-true (string-contains? output "process-attachment"))
          (check-true (string-contains? output "demo/stacktrace/liii_string.scm:20:"))
          (check-true (string-contains? output "handle-upload-job"))
          (check-false (string-contains? output "gfproject.scm"))
        ) ;let
      ) ;lambda
      (lambda () (path-unlink output-path #t))
    ) ;dynamic-wind
  ) ;let*

  ;; 3. 多文件跨模块协同调用栈测试
  (let* ((multi-demo-file (path "demo/stacktrace/multi_file.scm"))
         (output-path
           (path-join (path-temp-dir)
             (string-append "gf-stacktrace-multi-test-" (number->string (getpid)) ".log")
           ) ;path-join
         ) ;output-path
        ) ;
    (path-unlink output-path #t)
    (dynamic-wind (lambda () #t)
      (lambda ()
        (check-true (path-exists? multi-demo-file))
        (let ((cmd (string-append (executable)
                     " "
                     (path->string multi-demo-file)
                     " > "
                     (path->string output-path)
                     " 2>&1"
                   ) ;string-append
              ) ;cmd
             ) ;
          (check-true (not (= (run-shell-command cmd) 0)))
        ) ;let
        (let ((output (path-read-text output-path)))
          (check-true (string-contains? output "Stack trace:"))
          (check-true (string-contains? output "demo/stacktrace/engine.scm:3:"))
          (check-true (string-contains? output "compute-ratio"))
          (check-true (string-contains? output "demo/stacktrace/service.scm:4:"))
          (check-true (string-contains? output "format-score"))
          (check-true (string-contains? output "demo/stacktrace/multi_file.scm:4:"))
          (check-true (string-contains? output "process-user-record"))
          (check-true (string-contains? output "demo/stacktrace/multi_file.scm:9:"))
          (check-true (string-contains? output "run-batch"))
          (check-false (string-contains? output "gfproject.scm"))
        ) ;let
      ) ;lambda
      (lambda () (path-unlink output-path #t))
    ) ;dynamic-wind
  ) ;let*

  ;; 4. 递归树遍历与表达式求值调用栈测试
  (let* ((rec-demo-file (path "demo/stacktrace/recursive.scm"))
         (output-path
           (path-join (path-temp-dir)
             (string-append "gf-stacktrace-rec-test-" (number->string (getpid)) ".log")
           ) ;path-join
         ) ;output-path
        ) ;
    (path-unlink output-path #t)
    (dynamic-wind (lambda () #t)
      (lambda ()
        (check-true (path-exists? rec-demo-file))
        (let ((cmd (string-append (executable)
                     " "
                     (path->string rec-demo-file)
                     " > "
                     (path->string output-path)
                     " 2>&1"
                   ) ;string-append
              ) ;cmd
             ) ;
          (check-true (not (= (run-shell-command cmd) 0)))
        ) ;let
        (let ((output (path-read-text output-path)))
          (check-true (string-contains? output "Stack trace:"))
          (check-true (string-contains? output "demo/stacktrace/recursive.scm:18:"))
          (check-true (string-contains? output "eval-expr"))
          (check-true (string-contains? output "demo/stacktrace/recursive.scm:16:"))
          (check-false (string-contains? output "gfproject.scm"))
        ) ;let
      ) ;lambda
      (lambda () (path-unlink output-path #t))
    ) ;dynamic-wind
  ) ;let*

  ;; 5. 高阶函数数据清洗流水线回调调用栈测试
  (let* ((ho-demo-file (path "demo/stacktrace/higher_order.scm"))
         (output-path
           (path-join (path-temp-dir)
             (string-append "gf-stacktrace-ho-test-" (number->string (getpid)) ".log")
           ) ;path-join
         ) ;output-path
        ) ;
    (path-unlink output-path #t)
    (dynamic-wind (lambda () #t)
      (lambda ()
        (check-true (path-exists? ho-demo-file))
        (let ((cmd (string-append (executable)
                     " "
                     (path->string ho-demo-file)
                     " > "
                     (path->string output-path)
                     " 2>&1"
                   ) ;string-append
              ) ;cmd
             ) ;
          (check-true (not (= (run-shell-command cmd) 0)))
        ) ;let
        (let ((output (path-read-text output-path)))
          (check-true (string-contains? output "Stack trace:"))
          (check-true (string-contains? output "demo/stacktrace/higher_order.scm:16:"))
          (check-true (string-contains? output "extract-age"))
          (check-true (string-contains? output "demo/stacktrace/higher_order.scm:25:"))
          (check-true (string-contains? output "pipeline"))
          (check-false (string-contains? output "gfproject.scm"))
        ) ;let
      ) ;lambda
      (lambda () (path-unlink output-path #t))
    ) ;dynamic-wind
  ) ;let*

  ;; 6. 验证超长表达式截断 (object_to_string_truncated) 产物不包含 NUL 字节截断
  (let ((captured-info #f))
    (catch 'syntax-error
      (lambda ()
        (eval '(let* ((a 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)) 1))
        #f
      ) ;lambda
      (lambda (tag info) (set! captured-info info))
    ) ;catch
    (check-true (pair? captured-info))
    (let* ((err-msg (apply format #f (car captured-info) (cdr captured-info)))
           (chars (string->list err-msg))
           (has-null?
             (let loop
               ((lst chars))
               (cond ((null? lst) #f)
                     ((= (char->integer (car lst)) 0) #t)
                     (else (loop (cdr lst)))
               ) ;cond
             ) ;let
           ) ;has-null?
          ) ;
      (check-false has-null?)
      (check-true (string-contains? err-msg "..."))
    ) ;let*
  ) ;let
) ;when

(check-report)
