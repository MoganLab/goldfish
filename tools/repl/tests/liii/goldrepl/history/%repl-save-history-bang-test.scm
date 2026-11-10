;; %repl-save-history!
;; 把历史条目写入历史文件。
;;
;; 语法
;; ----
;; (%repl-save-history! entries old-count)
;;
;; 参数
;; ----
;; entries : list
;; 需要保存的历史条目列表。
;; old-count : integer
;; 载入历史时已有的条目数量，用于计算文件头部的总条目数。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未定义，函数通过写入历史文件生效。

(import (scheme base)
  (liii check)
  (liii list)
  (liii os)
  (liii path)
  (liii string)
  (liii goldrepl history)
) ;import

(check-set-mode! 'report-failed)

;; 把历史文件隔离到临时目录，避免影响用户环境
(let* ((tmp (string-append (os-temp-dir) "/goldrepl-history-save-test"))
       (old-xdg (getenv "XDG_DATA_HOME"))
       (old-appdata (getenv "APPDATA"))
      ) ;
  (putenv "XDG_DATA_HOME" tmp)
  (putenv "APPDATA" tmp)
  ;; 保存历史
  (%repl-save-history! '("(+ 1 2)" "(* 3 4)") 0)
  (let ((path (%repl-history-file)))
    ;; 历史文件已经生成
    (check (path-file? path) => #t)
    ;; 文件头部记录了总条目数
    (check (string-starts? (path-read-text path) "#!isocline 2") => #t)
    ;; 保存之后可以重新解析出同样的条目
    (let ((res (%repl-history-file-lines path)))
      (check (car res) => 2)
      (check (cdr res) => '("(+ 1 2)" "(* 3 4)"))
    ) ;let
  ) ;let
  ;; 条目中的换行会被转义，仍然只占一行
  (%repl-save-history! '("(display \"a\")\n(b)") 0)
  (let ((res (%repl-history-file-lines (%repl-history-file))))
    (check (car res) => 1)
    (check (cdr res) => '("(display \"a\")\\n(b)"))
  ) ;let
  (if old-xdg (putenv "XDG_DATA_HOME" old-xdg) (unsetenv "XDG_DATA_HOME"))
  (if old-appdata (putenv "APPDATA" old-appdata) (unsetenv "APPDATA"))
) ;let*

(check-report)
