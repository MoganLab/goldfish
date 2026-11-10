;; %repl-load-history!
;; 读取历史文件并载入历史对象，返回 (条目数 . 条目列表)。
;;
;; 语法
;; ----
;; (%repl-load-history! hist)
;;
;; 参数
;; ----
;; hist : history
;; 编辑器的历史对象。
;;
;; 返回值
;; ----
;; list
;; 二元组 `(条目数 . 条目列表)`，条目为反转义之后的原始文本。

(import (scheme base)
  (liii check)
  (liii list)
  (liii os)
  (liii path)
  (liii string)
  (liii tui)
  (liii goldrepl history)
) ;import

(check-set-mode! 'report-failed)

;; 把历史文件隔离到临时目录，避免影响用户环境
(let* ((tmp (string-append (os-temp-dir) "/goldrepl-history-load-test"))
       (old-xdg (getenv "XDG_DATA_HOME"))
       (old-appdata (getenv "APPDATA"))
      ) ;
  (putenv "XDG_DATA_HOME" tmp)
  (putenv "APPDATA" tmp)
  ;; 准备历史文件
  (path-mkdir (string-append tmp "/goldfish") #t #t)
  (let ((p (open-output-file (string-append tmp "/goldfish/history"))))
    (display "#!isocline 2\n" p)
    (display "(+ 1 2)\n" p)
    (display "(* 3 4)\n" p)
    (close-output-port p)
  ) ;let
  ;; 载入历史
  (let* ((ed (make-text-input))
         (hist (editor-history ed))
         (res (%repl-load-history! hist))
        ) ;
    (check (car res) => 2)
    (check (cdr res) => '("(+ 1 2)" "(* 3 4)"))
    ;; 历史对象中已经保存了两条记录
    (check (length (history-entries hist)) => 2)
    (check (any (lambda (e) (string=? e "(+ 1 2)")) (history-entries hist)) => #t)
    (check (any (lambda (e) (string=? e "(* 3 4)")) (history-entries hist)) => #t)
  ) ;let*
  (if old-xdg (putenv "XDG_DATA_HOME" old-xdg) (unsetenv "XDG_DATA_HOME"))
  (if old-appdata (putenv "APPDATA" old-appdata) (unsetenv "APPDATA"))
) ;let*

(check-report)
