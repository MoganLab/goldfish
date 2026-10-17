(import (scheme base)
        (scheme file)
        (liii check)
        (liii os)
        (liii path)
        (liii string)
        (liii string-cursor)
        (liii tempfile))

(check-set-mode! 'report-failed)

;; with-temp-file
;; 作用域安全临时文件执行，退出作用域时自动物理删除临时文件。
;;
;; 语法
;; ----
;; (with-temp-file proc [prefix [suffix [dir]]]) → any
;;
;; 参数
;; ----
;; proc : procedure?
;; 接受临时文件绝对路径为参数的过程 (lambda (path) ...)。
;; prefix : string? (可选，默认 "tmp")
;; 临时文件名前缀。
;; suffix : string? (可选，默认 "")
;; 临时文件名后缀。
;; dir : string? | #f (可选，默认 #f)
;; 目标存放父目录，为 #f 时使用系统临时目录。
;;
;; 返回值
;; -----
;; any
;; 返回 proc 调用的执行结果。
;;
;; 错误处理
;; ----
;; type-error 当 proc 非 procedure、prefix 非 string、suffix 非 string、dir 非 string 且非 #f 时。
;; file-not-found-error 当显式指定的 dir 不存在时。
;; permission-denied-error 当显式指定的 dir 无写权限时。

;; 测试项1：正常生命周期与自动清理
(let ((saved-path #f))
  (let ((ret (with-temp-file
               (lambda (p)
                 (set! saved-path p)
                 (check-true (file-exists? p))
                 (path-write-text p "scoped-content")
                 (check (path-read-text p) => "scoped-content")
                 'result-val))))
    (check ret => 'result-val)
    (check-false (file-exists? saved-path))))

;; 测试项2：异常中断下的动态清理保证（dynamic-wind）
(let ((saved-path #f))
  (guard (e (else 'caught))
    (with-temp-file
      (lambda (p)
        (set! saved-path p)
        (path-write-text p "will fail")
        (error 'test-intentional-error))))
  (check-true (string? saved-path))
  (check-false (file-exists? saved-path)))

;; 测试项3：自定义 prefix、suffix 与 dir
(let* ((parent-dir (make-temp-dir "scope-parent-"))
       (saved-path #f))
  (with-temp-file
    (lambda (p)
      (set! saved-path p)
      (check-true (string-contains? p "agent-"))
      (check-true (string-suffix? ".scm" p))
      (check-true (string-contains? p parent-dir))
      (check-true (file-exists? p)))
    "agent-"
    ".scm"
    parent-dir)
  (check-false (file-exists? saved-path))
  (rmdir parent-dir))

;; 测试项4：类型防御校验
(check-catch 'type-error (with-temp-file "not-a-proc"))
(check-catch 'type-error (with-temp-file 123))
(check-catch 'type-error (with-temp-file (lambda (p) p) 123))
(check-catch 'type-error (with-temp-file (lambda (p) p) "tmp" #f))
(check-catch 'type-error (with-temp-file (lambda (p) p) "tmp" "" 'symbol))

;; 测试项5：父目录有效性校验
(check-catch 'file-not-found-error
  (with-temp-file (lambda (p) p) "tmp" "" "/nonexistent/scope/dir"))

(check-report)
