;; (liii tempfile) 模块函数分类索引
;;
;; tempfile 提供跨平台原子的临时文件与目录安全创建与基于动态作用域的管理。
;; 对齐 Python tempfile 模块。

;; ==== 常见用法示例 ====
(import (liii tempfile))

;; 示例1：独占原子创建临时文件并写入
;; (let-values (((port path) (mkstemp "my-prefix-" ".log")))
;;   (display "data" port)
;;   (close-output-port port))

;; 示例2：独占原子创建临时目录
;; (let ((dir (mkdtemp "my-dir-"))) ...)

;; 示例3：作用域自动回收临时文件
;; (with-temp-file (lambda (path) ...))

;; 示例4：作用域自动递归回收临时目录
;; (with-temp-dir (lambda (dir) ...))

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/tempfile "mkstemp"
;;   bin/gf doc liii/tempfile "mkdtemp"
;;   bin/gf doc liii/tempfile "with-temp-file"
;;   bin/gf doc liii/tempfile "with-temp-dir"

;; ==== 函数分类索引 ====
;;   mkstemp         - 原子独占创建临时文件，返回端口与路径
;;   mkdtemp         - 原子独占创建临时目录，返回目录路径
;;   with-temp-file  - 动态作用域自动回收临时文件
;;   with-temp-dir   - 动态作用域自动递归回收临时目录

(import (scheme base)
        (scheme file)
        (liii check)
        (liii os)
        (liii path)
        (liii string)
        (liii tempfile))

(check-set-mode! 'report-failed)

;; =========================================================================
;; (liii tempfile) 模块集成测试与压力测试
;; =========================================================================

;; 1. 循环批量创建独占性测试：连续创建 20 个临时文件，验证路径互不重复且权限独占
(let ((paths '())
      (ports '()))
  (let loop ((i 0))
    (when (< i 20)
      (let-values (((port path) (mkstemp "bulk-" ".tmp")))
        (check-false (member path paths))
        (set! paths (cons path paths))
        (set! ports (cons port ports))
        (loop (+ i 1)))))
  (check (= (length paths) 20) => #t)
  (for-each (lambda (p) (close-output-port p)) ports)
  (for-each (lambda (path)
              (unless (os-windows?)
                (check (= (g_file-mode path) #o600) => #t))
              (delete-file path)
              (check-false (file-exists? path)))
            paths))

;; 2. 循环批量创建目录独占性测试：连续创建 20 个临时目录，验证互不冲突
(let ((dirs '()))
  (let loop ((i 0))
    (when (< i 20)
      (let ((d (mkdtemp "bulk-dir-")))
        (check-false (member d dirs))
        (set! dirs (cons d dirs))
        (loop (+ i 1)))))
  (check (= (length dirs) 20) => #t)
  (for-each (lambda (d)
              (unless (os-windows?)
                (check (= (g_file-mode d) #o700) => #t))
              (rmdir d)
              (check-false (file-exists? d)))
            dirs))

;; 3. 嵌套作用域管理：with-temp-dir 内部嵌套 with-temp-file
(let ((saved-dir #f)
      (saved-file #f))
  (with-temp-dir
    (lambda (d)
      (set! saved-dir d)
      (with-temp-file
        (lambda (f)
          (set! saved-file f)
          (check-true (string-contains? f d))
          (path-write-text f "nested file inside temp dir")
          (check (path-read-text f) => "nested file inside temp dir"))
        "inner-"
        ".data"
        d)
      ;; 离开 with-temp-file 后，文件已被删除
      (check-false (file-exists? saved-file))
      ;; 在目录中写入其他文件
      (path-write-text (path-join d "other.txt") "other")))
  ;; 离开 with-temp-dir 后，整个目录包括 other.txt 全被清除
  (check-false (file-exists? saved-dir)))

;; 4. 异常回溯深层清理测试：嵌套作用域中途抛出异常
(let ((outer-dir #f)
      (inner-dir #f)
      (inner-file #f))
  (guard (e (else 'caught-nested-abort))
    (with-temp-dir
      (lambda (d1)
        (set! outer-dir d1)
        (with-temp-dir
          (lambda (d2)
            (set! inner-dir d2)
            (with-temp-file
              (lambda (f)
                (set! inner-file f)
                (path-write-text f "data before failure")
                (error 'nested-deep-error))
              "f-"
              ".txt"
              d2))
          "d2-"
          ""
          d1))))
  (check-false (file-exists? inner-file))
  (check-false (file-exists? inner-dir))
  (check-false (file-exists? outer-dir)))

;; 5. 大数据日志转储与校验：通过 port 写入 5000 行并验证完整性
(let-values (((port path) (mkstemp "large-log-" ".log")))
  (let loop ((i 0))
    (when (< i 5000)
      (display "log line record index: " port)
      (display i port)
      (newline port)
      (loop (+ i 1))))
  (close-output-port port)
  (let ((in (open-input-file path))
        (count 0))
    (let loop ()
      (let ((line (read-line in)))
        (unless (eof-object? line)
          (set! count (+ count 1))
          (loop))))
    (close-input-port in)
    (check count => 5000))
  (delete-file path)
  (check-false (file-exists? path)))

(check-report)
