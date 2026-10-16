(import (scheme base)
        (scheme file)
        (liii check)
        (liii os)
        (liii string)
        (liii string-cursor)
        (liii tempfile))

(check-set-mode! 'report-failed)

;; mkdtemp
;; 原子独占创建临时目录，返回目录绝对路径。
;;
;; 语法
;; ----
;; (mkdtemp [prefix [suffix [dir]]]) → string
;;
;; 参数
;; ----
;; prefix : string? (可选，默认 "tmp")
;; 临时目录名前缀。
;; suffix : string? (可选，默认 "")
;; 临时目录名后缀。
;; dir : string? | #f (可选，默认 #f)
;; 目标存放父目录，为 #f 时使用系统临时目录。
;;
;; 返回值
;; -----
;; string?
;; 返回原子独占创建的临时目录绝对路径。
;;
;; 错误处理
;; ----
;; type-error 当 prefix 非 string、suffix 非 string、dir 非 string 且非 #f 时。
;; file-not-found-error 当显式指定的 dir 不存在时。
;; permission-denied-error 当显式指定的 dir 无写权限时。

;; 测试项1：默认参数原子创建临时目录
(let ((dir (mkdtemp)))
  (check-true (string? dir))
  (check-true (file-exists? dir))
  (check-true (g_isdir dir))
  (unless (os-windows?)
    (check (= (g_file-mode dir) #o700) => #t))
  ;; 在其中创建子文件
  (let ((subfile (string-append dir (string (os-sep)) "child.txt")))
    (let ((p (open-output-file subfile)))
      (display "dir test data" p)
      (close-output-port p))
    (check-true (file-exists? subfile))
    (delete-file subfile))
  (rmdir dir)
  (check-false (file-exists? dir)))

;; 测试项2：自定义 prefix 与 suffix
(let ((dir (mkdtemp "my-tmp-dir-" "-tail")))
  (check-true (file-exists? dir))
  (check-true (string-contains? dir "my-tmp-dir-"))
  (check-true (string-suffix? "-tail" dir))
  (rmdir dir)
  (check-false (file-exists? dir)))

;; 测试项3：自定义父目录 dir
(let* ((parent-dir (mkdtemp "parent-dir-"))
       (sub-dir (mkdtemp "sub-dir-" "" parent-dir)))
  (check-true (file-exists? sub-dir))
  (check-true (string-contains? sub-dir parent-dir))
  (rmdir sub-dir)
  (rmdir parent-dir)
  (check-false (file-exists? parent-dir)))

;; 测试项4：类型防御校验
(check-catch 'type-error (mkdtemp 123))
(check-catch 'type-error (mkdtemp "tmp" #f))
(check-catch 'type-error (mkdtemp "tmp" "" 'symbol))

;; 测试项5：父目录不存在与无写权限校验
(check-catch 'file-not-found-error (mkdtemp "tmp" "" "/nonexistent/dir/1052"))

(unless (or (os-windows?) (equal? (getlogin) "root"))
  (let ((ro-dir (mkdtemp "ro-dir-")))
    (g_chmod ro-dir #o400)
    (check-catch 'permission-denied-error (mkdtemp "tmp" "" ro-dir))
    (g_chmod ro-dir #o700)
    (rmdir ro-dir)))

(check-report)
