;; %repl-cleanup!
;; 退出 REPL 时保存历史并恢复终端状态。
;;
;; 语法
;; ----
;; (%repl-cleanup! term screen hist-entries old-count)
;;
;; 参数
;; ----
;; term : terminal
;; 终端对象。
;; screen : screen
;; 主屏幕对象。
;; hist-entries : list
;; 本次会话新增的历史条目列表。
;; old-count : integer
;; 载入历史时已有的条目数量。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未定义，函数通过保存历史与恢复终端生效。

(import (scheme base)
  (liii check)
  (liii list)
  (liii os)
  (liii string)
  (liii tui)
  (liii goldrepl history)
  (liii goldrepl loop)
) ;import

(check-set-mode! 'report-failed)

;; 把历史文件隔离到临时目录，避免影响用户环境
(let* ((tmp (string-append (os-temp-dir) "/goldrepl-cleanup-test"))
       (old-xdg (getenv "XDG_DATA_HOME"))
       (old-appdata (getenv "APPDATA"))
       (vt (make-virtual-terminal :width 60 :height 10))
       (screen (make-main-screen vt))
      ) ;
  (putenv "XDG_DATA_HOME" tmp)
  (putenv "APPDATA" tmp)
  ;; 清理过程不会抛出异常
  (check
    (catch #t
      (lambda () (%repl-cleanup! vt screen '("(+ 1 2)") 0) #t)
      (lambda (t i) #f)
    ) ;catch
    =>
    #t
  ) ;check
  ;; 历史条目已经写入历史文件
  (check-true
    (any (lambda (e) (string=? e "(+ 1 2)"))
      (cdr (%repl-history-file-lines (%repl-history-file)))
    ) ;any
  ) ;check-true
  (if old-xdg (putenv "XDG_DATA_HOME" old-xdg) (unsetenv "XDG_DATA_HOME"))
  (if old-appdata (putenv "APPDATA" old-appdata) (unsetenv "APPDATA"))
) ;let*

(check-report)
