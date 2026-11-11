;; %parallel-result
;; 处理并行测试结果并输出单行进度与结果。
;;
;; 语法
;; ----
;; (%parallel-result res test-file tmo)
;; (%parallel-result idx total res test-file tmo)
;;
;; 参数
;; ----
;; idx : integer? 测试文件序号（0-based）
;; total : integer? 测试文件总数
;; res : either? 子进程执行结果
;; test-file : string? 测试文件路径
;; tmo : integer? 超时时间（秒）
;;
;; 返回值
;; ------
;; list? 包含 (list test-file exit-code "")

(import (scheme base)
  (liii check)
  (liii base)
  (liii string)
  (liii either)
  (liii goldtest-runner)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? %parallel-result))

(check-true
  (let ((out
          (with-output-to-string (lambda () (%parallel-result (from-right 0) "foo.scm" 300))
          ) ;with-output-to-string
        ) ;out
       ) ;
    (and (string-contains out "foo.scm ... ") (string-contains out "PASS"))
  ) ;let
) ;check-true

(check-true
  (let ((out
          (with-output-to-string (lambda () (%parallel-result 0 10 (from-right 0) "foo.scm" 300))
          ) ;with-output-to-string
        ) ;out
       ) ;
    (and (string-contains out "[1/10] foo.scm ... ") (string-contains out "PASS"))
  ) ;let
) ;check-true

(check-report)
