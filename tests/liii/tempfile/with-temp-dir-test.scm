(import (scheme base)
        (scheme file)
        (liii check)
        (liii os)
        (liii path)
        (liii string)
        (liii string-cursor)
        (liii tempfile))

(check-set-mode! 'report-failed)

;; with-temp-dir
;; 作用域安全临时目录执行，退出作用域时递归删除整个目录树。
;;
;; 语法
;; ----
;; (with-temp-dir proc [prefix [suffix [dir]]]) → any
;;
;; 参数
;; ----
;; proc : procedure?
;; 接受临时目录绝对路径为参数的过程 (lambda (dir-path) ...)。
;; prefix : string? (可选，默认 "tmp")
;; 临时目录名前缀。
;; suffix : string? (可选，默认 "")
;; 临时目录名后缀。
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

;; 测试项1：正常生命周期与单层子文件清理
(let ((saved-dir #f))
  (let ((ret (with-temp-dir
               (lambda (d)
                 (set! saved-dir d)
                 (check-true (file-exists? d))
                 (check-true (g_isdir d))
                 (let ((f (string-append d (string (os-sep)) "sub.txt")))
                   (path-write-text f "data")
                   (check-true (file-exists? f)))
                 'dir-ok))))
    (check ret => 'dir-ok)
    (check-false (file-exists? saved-dir))))

;; 测试项2：多层嵌套子目录与文件的递归自动回收
(let ((saved-dir #f))
  (with-temp-dir
    (lambda (d)
      (set! saved-dir d)
      (let* ((sep (string (os-sep)))
             (sub-a (string-append d sep "subA"))
             (sub-b (string-append sub-a sep "subB"))
             (file-1 (string-append sub-a sep "f1.txt"))
             (file-2 (string-append sub-b sep "f2.txt")))
        (mkdir sub-a)
        (mkdir sub-b)
        (path-write-text file-1 "nested file 1")
        (path-write-text file-2 "nested file 2")
        (check-true (file-exists? file-1))
        (check-true (file-exists? file-2)))))
  (check-false (file-exists? saved-dir)))

;; 测试项3：异常中断下的多层目录递归动态回收（dynamic-wind）
(let ((saved-dir #f))
  (guard (e (else 'caught))
    (with-temp-dir
      (lambda (d)
        (set! saved-dir d)
        (let* ((sep (string (os-sep)))
               (sub (string-append d sep "sub"))
               (file (string-append sub sep "file.txt")))
          (mkdir sub)
          (path-write-text file "abort data")
          (error 'test-dir-abort-error)))))
  (check-true (string? saved-dir))
  (check-false (file-exists? saved-dir)))

;; 测试项4：自定义 prefix、suffix 与 dir
(let* ((parent-dir (mkdtemp "scope-parent-dir-"))
       (saved-dir #f))
  (with-temp-dir
    (lambda (d)
      (set! saved-dir d)
      (check-true (string-contains? d "my-agent-"))
      (check-true (string-suffix? ".dir" d))
      (check-true (string-contains? d parent-dir))
      (check-true (file-exists? d)))
    "my-agent-"
    ".dir"
    parent-dir)
  (check-false (file-exists? saved-dir))
  (rmdir parent-dir))

;; 测试项5：类型防御校验
(check-catch 'type-error (with-temp-dir "not-proc"))
(check-catch 'type-error (with-temp-dir 456))
(check-catch 'type-error (with-temp-dir (lambda (d) d) 123))
(check-catch 'type-error (with-temp-dir (lambda (d) d) "tmp" #f))
(check-catch 'type-error (with-temp-dir (lambda (d) d) "tmp" "" 'symbol))

;; 测试项6：父目录有效性校验
(check-catch 'file-not-found-error
  (with-temp-dir (lambda (d) d) "tmp" "" "/nonexistent/scope/dir/path"))

(check-report)
