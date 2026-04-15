(import (liii check)
  (liii path)
  (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-exists?
;; 判断给定路径是否存在。
;;
;; 语法
;; ----
;; (path-exists? path)
;;
;; 参数
;; ----
;; path : string? | path-value
;; 文件或目录路径。
;;
;; 返回值
;; ----
;; boolean
;; 当路径存在时返回 #t，路径不存在时返回 #f。

;; 基本功能测试
(check-true (path-exists? "."))
(check-true (path-exists? ".."))
(check-true (path-exists? (path ".")))

;; 边界情况测试
(check (path-exists? "") => #f)
(check (path-exists? "nonexistent")
  =>
  #f
) ;check
(check (path-exists? "#/null") => #f)

;; 系统路径测试
(when (not (os-windows?))
  (check-true (path-exists? "/"))
  (check-true (path-exists? "/etc"))
  (check-true (path-exists? "/etc/passwd")
  ) ;check-true
  (check (path-exists? "/no_such_file")
    =>
    #f
  ) ;check
  (check (path-exists? "/not/a/real/path")
    =>
    #f
  ) ;check
) ;when

(when (os-windows?)
  (check-true (path-exists? "C:/"))
  (check-true (path-exists? "C:/Windows"))
  (check-true (path-exists? "C:\\Windows\\System32\\drivers\\etc\\hosts"
              ) ;path-exists?
  ) ;check-true
  (check (path-exists? "C:\\Windows\\InvalidPath"
         ) ;path-exists?
    =>
    #f
  ) ;check
) ;when

(check-report)
