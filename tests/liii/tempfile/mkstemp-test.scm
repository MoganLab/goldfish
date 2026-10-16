(import (scheme base)
        (scheme file)
        (liii check)
        (liii os)
        (liii string)
        (liii string-cursor)
        (liii tempfile))

(check-set-mode! 'report-failed)

;; mkstemp
;; 原子独占创建临时文件，返回已打开的 Scheme 输出端口与文件绝对路径。
;;
;; 语法
;; ----
;; (mkstemp [prefix [suffix [dir]]]) → (values port path)
;;
;; 参数
;; ----
;; prefix : string? (可选，默认 "tmp")
;; 临时文件名前缀。
;; suffix : string? (可选，默认 "")
;; 临时文件名后缀（如扩展名 ".log"）。
;; dir : string? | #f (可选，默认 #f)
;; 目标存放父目录，为 #f 时使用系统临时目录。
;;
;; 返回值
;; -----
;; (values port path)
;; port : output-port?，已打开的可写文件端口。
;; path : string?，原子创建的临时文件绝对路径。
;;
;; 错误处理
;; ----
;; type-error 当 prefix 非 string、suffix 非 string、dir 非 string 且非 #f 时。
;; file-not-found-error 当显式指定的 dir 不存在时。
;; permission-denied-error 当显式指定的 dir 无写权限时。

;; 测试项1：默认参数原子创建与写入
(let-values (((port path) (mkstemp)))
  (check-true (output-port? port))
  (check-true (string? path))
  (check-true (file-exists? path))
  (unless (os-windows?)
    (check (= (g_file-mode path) #o600) => #t))
  (display "hello mkstemp" port)
  (newline port)
  (close-output-port port)
  (let ((in (open-input-file path)))
    (check (read-line in) => "hello mkstemp")
    (close-input-port in))
  (delete-file path)
  (check-false (file-exists? path)))

;; 测试项2：自定义 prefix 与 suffix
(let-values (((port path) (mkstemp "test-" ".log")))
  (check-true (output-port? port))
  (check-true (string-contains? path "test-"))
  (check-true (string-suffix? ".log" path))
  (check-true (file-exists? path))
  (close-output-port port)
  (delete-file path)
  (check-false (file-exists? path)))

;; 测试项3：自定义父目录 dir
(let* ((parent-dir (mkdtemp "parent-tmp-"))
       (res (let-values (((port path) (mkstemp "sub-" ".txt" parent-dir)))
              (close-output-port port)
              (delete-file path)
              (string-contains? path parent-dir))))
  (rmdir parent-dir)
  (check-true res))

;; 测试项4：类型防御校验
(check-catch 'type-error (mkstemp 123))
(check-catch 'type-error (mkstemp "a" #t))
(check-catch 'type-error (mkstemp "a" "b" 456))

;; 测试项5：父目录不存在与无写权限校验
(check-catch 'file-not-found-error (mkstemp "tmp" "" "/nonexistent/temp/path/1052"))

(unless (os-windows?)
  (let ((ro-dir (mkdtemp "ro-dir-")))
    (g_chmod ro-dir #o400)
    (check-catch 'permission-denied-error (mkstemp "tmp" "" ro-dir))
    (g_chmod ro-dir #o700)
    (rmdir ro-dir)))

(check-report)
