(import (liii check) (liii os) (liii path) (liii string) (liii sys))

(check-set-mode! 'report-failed)

(define (run-shell-command command)
  (os-call (string-append "sh -c \"" command "\""))
) ;define

(when (not (os-windows?))
  ;; 运行 demo 脚本（预期报错退出），校验 stack trace 输出包含期望的
  ;; 文件名/行号/函数名，且不泄漏 gfproject.scm 内部加载帧
  (define (check-demo-stacktrace name demo-file expected-substrings)
    (let ((output-path
            (path-join (path-temp-dir)
              (string-append "gf-stacktrace-" name "-test-" (number->string (getpid)) ".log")
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
            (for-each
              (lambda (expected) (check-true (string-contains? output expected)))
              expected-substrings
            ) ;for-each
            (check-false (string-contains? output "gfproject.scm"))
          ) ;let
        ) ;lambda
        (lambda () (path-unlink output-path #t))
      ) ;dynamic-wind
    ) ;let
  ) ;define

  ;; 1. 单文件简单调用栈测试
  (check-demo-stacktrace "simple" (path "demo/stacktrace/simple.scm")
    (list "demo/stacktrace/simple.scm:3:" "step-c"
          "demo/stacktrace/simple.scm:7:" "step-b"
          "demo/stacktrace/simple.scm:12:" "step-a"
    ) ;list
  ) ;check-demo-stacktrace

  ;; 2. 标准库 (liii string) 输入类型检查报错调用栈测试
  (check-demo-stacktrace "string" (path "demo/stacktrace/liii_string.scm")
    (list "goldfish/liii/string.scm:44:" "string-remove-suffix"
          "demo/stacktrace/liii_string.scm:8:" "clean-file-extension"
          "demo/stacktrace/liii_string.scm:14:" "process-attachment"
          "demo/stacktrace/liii_string.scm:20:" "handle-upload-job"
    ) ;list
  ) ;check-demo-stacktrace

  ;; 3. 多文件跨模块协同调用栈测试
  (check-demo-stacktrace "multi" (path "demo/stacktrace/multi_file.scm")
    (list "demo/stacktrace/engine.scm:3:" "compute-ratio"
          "demo/stacktrace/service.scm:4:" "format-score"
          "demo/stacktrace/multi_file.scm:4:" "process-user-record"
          "demo/stacktrace/multi_file.scm:9:" "run-batch"
    ) ;list
  ) ;check-demo-stacktrace

  ;; 4. 递归树遍历与表达式求值调用栈测试
  (check-demo-stacktrace "rec" (path "demo/stacktrace/recursive.scm")
    (list "demo/stacktrace/recursive.scm:18:" "eval-expr"
          "demo/stacktrace/recursive.scm:16:"
    ) ;list
  ) ;check-demo-stacktrace

  ;; 5. 高阶函数数据清洗流水线回调调用栈测试
  (check-demo-stacktrace "ho" (path "demo/stacktrace/higher_order.scm")
    (list "demo/stacktrace/higher_order.scm:16:" "extract-age"
          "demo/stacktrace/higher_order.scm:25:" "pipeline"
    ) ;list
  ) ;check-demo-stacktrace

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
           ;; string-contains? 对 NUL 不安全，需逐字符检查
           (has-null?
             (let loop
               ((lst (string->list err-msg)))
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
