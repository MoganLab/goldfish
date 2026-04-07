(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-touch
;; 创建或更新时间戳文件。
;;
;; 语法
;; ----
;; (path-touch path) → boolean
;;
;; 返回值
;; -----
;; boolean
;; 返回 #t 表示操作成功完成，返回 #f 表示操作失败。
;;
;; 说明
;; ----
;; path-touch 不会自动创建父目录。如果父目录不存在，返回 #f。

;; 创建新空文件测试
(let ((touch-file (path-join (path-temp-dir) "path-touch-basic.txt")))
  (when (path-exists? touch-file)
    (delete-file (path->string touch-file))
  ) ;when
  (check-false (path-exists? touch-file))
  (check-true (path-touch touch-file))
  (check-true (path-exists? touch-file))
  (check-true (path-file? touch-file))
  (check (path-getsize touch-file) => 0)
  (delete-file (path->string touch-file))
) ;let

;; 更新现有文件时间戳测试
(let ((touch-file (path-join (path-temp-dir) "path-touch-update.txt")))
  (when (path-exists? touch-file)
    (delete-file (path->string touch-file))
  ) ;when
  (path-write-text touch-file "keep")
  (check-true (path-touch touch-file))
  (check (path-read-text touch-file) => "keep")
  (delete-file (path->string touch-file))
) ;let

;; 目录时间戳测试
(let ((touch-dir (path-join (path-temp-dir) "path-touch-dir")))
  (when (path-exists? touch-dir)
    (rmdir (path->string touch-dir))
  ) ;when
  (mkdir (path->string touch-dir))
  (check-true (path-touch touch-dir))
  (check-true (path-dir? touch-dir))
  (rmdir (path->string touch-dir))
) ;let

;; 特殊文件名测试
(let ((special-file (path-join (path-temp-dir) "path-touch-special_中文#.txt")))
  (when (path-exists? special-file)
    (delete-file (path->string special-file))
  ) ;when
  (check-true (path-touch special-file))
  (check-true (path-exists? special-file))
  (delete-file (path->string special-file))
) ;let

;; 相对路径与重复调用测试
(let ((relative-file (path "path-touch-relative.txt")))
  (when (path-exists? relative-file)
    (delete-file (path->string relative-file))
  ) ;when
  (check-true (path-touch relative-file))
  (check-true (path-touch relative-file))
  (check-true (path-touch relative-file))
  (check-true (path-exists? relative-file))
  (delete-file (path->string relative-file))
) ;let

(check-report)
