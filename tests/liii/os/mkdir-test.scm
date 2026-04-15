(import (liii check) (liii os))


(check-set-mode! 'report-failed)


;; mkdir
;; 创建目录。
;;
;; 语法
;; ----
;; (mkdir path)
;;
;; 参数
;; ----
;; path : string?
;; 要创建的目录路径。
;;
;; 返回值
;; -----
;; boolean?
;; 成功返回 #t。
;;
;; 错误
;; ----
;; file-exists-error
;; 当目录已存在时抛出错误。
;;
;; 说明
;; ----
;; 创建单个目录，如果父目录不存在会失败。


;; ; 基本功能测试
(when (not (os-windows?))
  ;; 测试创建已存在的目录会报错
  (check-catch 'file-exists-error
    (mkdir "/tmp")
  ) ;check-catch

  ;; 测试创建新目录
  (check (begin
           (let ((test_dir "/tmp/test_124"))
             (when (file-exists? test_dir)
               (rmdir "/tmp/test_124")
             ) ;when
             (mkdir "/tmp/test_124")
           ) ;let
         ) ;begin
    =>
    #t
  ) ;check

  ;; 清理
  (when (file-exists? "/tmp/test_124")
    (rmdir "/tmp/test_124")
  ) ;when
) ;when


(check-report)
