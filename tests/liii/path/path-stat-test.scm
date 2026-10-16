(import (liii check) (liii path) (liii os) (liii time))

(check-set-mode! 'report-failed)

;; path-stat
;; 获取文件或目录的元数据记录对象（包含 size、mtime、atime、ctime、mode）。
;;
;; 语法
;; ----
;; (path-stat path) → <path-stat>
;;
;; 参数
;; ----
;; path : string | path
;; 要获取元数据的文件或目录路径。
;;
;; 返回值
;; -----
;; <path-stat>
;; 返回 <path-stat> 记录对象。
;;
;; 错误处理
;; ----
;; file-not-found-error 当目标路径不存在时。
;; type-error 当参数不是 string 或 path 记录时。

;; 测试项1：获取常规文件完整元数据与字段正确性
(let ((test-file (path-join (path-temp-dir) "path-stat-test-1050.txt")))
  (when (path-exists? test-file)
    (path-unlink test-file)
  ) ;when
  ;; 写入 11 字节文本 "hello world"
  (path-write-text test-file "hello world")
  (let ((st (path-stat test-file)) (st-str (path-stat (path->string test-file))))
    (check-true (path-stat? st))
    (check-true (path-stat? st-str))
    ;; 验证大小精确为 11 字节
    (check (path-stat-size st) => 11)
    (check (path-stat-size st-str) => 11)
    ;; 验证时间戳均为大于 0 的数值
    (check-true (> (path-stat-mtime st) 0))
    (check-true (> (path-stat-atime st) 0))
    (check-true (> (path-stat-ctime st) 0))
    ;; 验证 mode 为正整数
    (check-true (> (path-stat-mode st) 0))
    ;; 测试项2：常规文件快捷谓词测试
    (check-true (path-stat-file? st))
    (check-false (path-stat-dir? st))
    (check-false (path-stat-symlink? st))

    ;; 测试项3：mtime 时间戳敏感度与外部并发修改感知
    (let ((old-mtime (path-stat-mtime st)))
      (sleep 0.05)
      (path-append-text test-file " modified")
      (let* ((st-new (path-stat test-file)) (new-mtime (path-stat-mtime st-new)))
        (check-true (> new-mtime old-mtime))
        (check (path-stat-size st-new) => 20)
      ) ;let*
    ) ;let
  ) ;let
  (path-unlink test-file)
) ;let

;; 测试项2（续）：目录元数据与谓词判定测试
(let* ((tmp-dir (path-temp-dir)) (st-dir (path-stat tmp-dir)))
  (check-true (path-stat? st-dir))
  (check-true (path-stat-dir? st-dir))
  (check-false (path-stat-file? st-dir))
  (check-false (path-stat-symlink? st-dir))
  (check-true (> (path-stat-mtime st-dir) 0))
  (check-true (> (path-stat-mode st-dir) 0))
) ;let*

;; 测试项2（续）：软链接判定测试（支持软链接的 POSIX 平台）
(when (not (os-windows?))
  (let ((target-file (path-join (path-temp-dir) "path-stat-target-1050.txt"))
        (link-file (path-join (path-temp-dir) "path-stat-link-1050.txt"))
       ) ;
    (when (path-exists? link-file)
      (path-unlink link-file)
    ) ;when
    (when (path-exists? target-file)
      (path-unlink target-file)
    ) ;when
    (path-write-text target-file "target content")
    (os-call (string-append "ln -s " (path->string target-file) " " (path->string link-file))
    ) ;os-call
    (let ((st-link (path-stat link-file)))
      (check-true (path-stat? st-link))
      (check-true (path-stat-symlink? st-link))
      (check-false (path-stat-file? st-link))
      (check-false (path-stat-dir? st-link))
    ) ;let
    (path-unlink link-file)
    (path-unlink target-file)
  ) ;let
) ;when

;; 测试项4：错误处理与防御性健壮性测试
;; 1. 不存在的文件路径抛出 file-not-found-error
(check-catch 'file-not-found-error
  (path-stat "non-existent-path-for-1050-test-xyz")
) ;check-catch
(check-catch 'file-not-found-error
  (path-stat (path "non-existent-path-for-1050-test-xyz"))
) ;check-catch

;; 2. 非合法路径类型抛出 type-error
(check-catch 'type-error (path-stat 123))
(check-catch 'type-error (path-stat '("not" "a" "path")))
(check-catch 'type-error (path-stat #t))
(check-catch 'type-error (path-stat (vector 1 2 3)))

;; 3. 访问器与谓词对非 <path-stat> 对象抛出 type-error
(check-catch 'type-error (path-stat-size 123))
(check-catch 'type-error (path-stat-size "not-a-record"))
(check-catch 'type-error (path-stat-mtime 123))
(check-catch 'type-error (path-stat-atime 123))
(check-catch 'type-error (path-stat-ctime 123))
(check-catch 'type-error (path-stat-mode 123))
(check-catch 'type-error (path-stat-file? 123))
(check-catch 'type-error (path-stat-dir? 123))
(check-catch 'type-error (path-stat-symlink? 123))

;; 4. 显式构造函数 make-path-stat 测试
(let ((custom-st (make-path-stat 42 100.5 200.5 300.5 33188)))
  (check-true (path-stat? custom-st))
  (check (path-stat-size custom-st) => 42)
  (check (path-stat-mtime custom-st) => 100.5)
  (check (path-stat-atime custom-st) => 200.5)
  (check (path-stat-ctime custom-st) => 300.5)
  (check (path-stat-mode custom-st) => 33188)
  (check-true (path-stat-file? custom-st))
  (check-false (path-stat-dir? custom-st))
  (check-false (path-stat-symlink? custom-st))
) ;let

(check-report)
