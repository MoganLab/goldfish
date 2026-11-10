;; %repl-history-dir
;; 返回历史文件所在目录，遵循 XDG_DATA_HOME（Windows 为 APPDATA）约定。
;;
;; 语法
;; ----
;; (%repl-history-dir)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受参数。
;;
;; 返回值
;; ----
;; string
;; 历史文件所在目录的路径。

(import (scheme base)
  (liii check)
  (liii os)
  (liii string)
  (liii goldrepl history)
) ;import

(check-set-mode! 'report-failed)

;; 返回的是字符串路径
(check (string? (%repl-history-dir)) => #t)

;; 指定 XDG_DATA_HOME 之后，历史目录位于该目录下的 goldfish 子目录
(let* ((tmp (string-append (os-temp-dir) "/goldrepl-history-dir-test"))
       (old-xdg (getenv "XDG_DATA_HOME"))
       (old-appdata (getenv "APPDATA"))
      ) ;
  (putenv "XDG_DATA_HOME" tmp)
  (putenv "APPDATA" tmp)
  (let ((dir (%repl-history-dir)))
    (check (string? dir) => #t)
    (check (string-contains dir "goldfish") => #t)
    (check (string-starts? dir tmp) => #t)
  ) ;let
  (if old-xdg (putenv "XDG_DATA_HOME" old-xdg) (unsetenv "XDG_DATA_HOME"))
  (if old-appdata (putenv "APPDATA" old-appdata) (unsetenv "APPDATA"))
) ;let*

(check-report)
