(import (liii check)
        (liii os)
        (liii uuid)
        (liii base)
        (liii oop)
        (liii lang)
) ;import

(check-set-mode! 'report-failed)

;; listdir
;; 列出目录中的内容。
;;
;; 语法
;; ----
;; (listdir path)
;;
;; 参数
;; ----
;; path : string?
;; 要列出的目录路径。
;;
;; 返回值
;; -----
;; vector?
;; 返回包含目录内容的向量。
;;
;; 说明
;; ----
;; 返回指定目录中的文件和子目录列表（不包括 . 和 ..）。

;;; 基本功能测试
(when (not (os-windows?))
  (check (> (vector-length (listdir "/usr")) 0) => #t)
) ;when

;;; 测试创建目录并列出
(let* ((test-dir (string-append (os-temp-dir) (string (os-sep)) (uuid4)))
       (test-dir2 (string-append test-dir (string (os-sep))))
       (dir-a (string-append test-dir2 "a"))
       (dir-b (string-append test-dir2 "b"))
       (dir-c (string-append test-dir2 "c")))
  (mkdir test-dir)
  (mkdir dir-a)
  (mkdir dir-b)
  (mkdir dir-c)
  (let ((r (listdir test-dir)))
    (check-true ($ r :contains "a"))
    (check-true ($ r :contains "b"))
    (check-true ($ r :contains "c"))
  ) ;let
  (let ((r2 (listdir test-dir2)))
    (check-true ($ r2 :contains "a"))
    (check-true ($ r2 :contains "b"))
    (check-true ($ r2 :contains "c"))
  ) ;let
  (rmdir dir-a)
  (rmdir dir-b)
  (rmdir dir-c)
  (rmdir test-dir)
) ;let*

(when (os-windows?)
  (check (> (vector-length (listdir "C:")) 0) => #t)
) ;when

(check-report)
