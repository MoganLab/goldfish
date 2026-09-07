(import (liii check) (liii os) (liii uuid) (liii base) (liii vector))


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


;; ; 基本功能测试
(when (not (os-windows?))
  (check (> (vector-length (listdir "/usr")) 0) => #t)
) ;when


;; ; 测试创建目录并列出
(let* ((test-dir (string-append (os-temp-dir) (string (os-sep)) (uuid4)))
       (test-dir2 (string-append test-dir (string (os-sep))))
       (dir-a (string-append test-dir2 "a"))
       (dir-b (string-append test-dir2 "b"))
       (dir-c (string-append test-dir2 "c"))
      ) ;
  (mkdir test-dir)
  (mkdir dir-a)
  (mkdir dir-b)
  (mkdir dir-c)
  (let ((r (listdir test-dir)))
    (check-true (vector-contains? r "a"))
    (check-true (vector-contains? r "b"))
    (check-true (vector-contains? r "c"))
  ) ;let
  (let ((r2 (listdir test-dir2)))
    (check-true (vector-contains? r2 "a"))
    (check-true (vector-contains? r2 "b"))
    (check-true (vector-contains? r2 "c"))
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


;; ; C 层入口 g_listdir 参数类型测试
;; path 必须是 string?，C++ 层需要类型守卫，
;; 避免把非字符串对象当作 C 字符串指针导致崩溃 (devel/0145.md)
(check-catch 'wrong-type-arg (g_listdir 99))
(check-catch 'wrong-type-arg (g_listdir 'dir))
(check-catch 'wrong-type-arg (g_listdir #t))
