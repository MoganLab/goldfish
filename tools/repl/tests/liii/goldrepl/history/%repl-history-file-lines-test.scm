;; %repl-history-file-lines
;; 解析历史文件，返回 (条目数 . 条目列表)。
;;
;; 语法
;; ----
;; (%repl-history-file-lines path)
;;
;; 参数
;; ----
;; path : string
;; 历史文件的路径。
;;
;; 返回值
;; ----
;; list
;; 二元组 `(条目数 . 条目列表)`；文件不存在时条目数为 0 且条目列表为空。

(import (scheme base)
  (liii check)
  (liii list)
  (liii os)
  (liii string)
  (liii goldrepl history)
) ;import

(check-set-mode! 'report-failed)

;; 历史文件解析（isocline 格式）
(let* ((tmp (string-append (os-temp-dir)
              "/goldrepl-history-test-"
              (number->string (getpid))
              ".txt"
            ) ;string-append
       ) ;tmp
       (p (open-output-file tmp))
      ) ;
  (display "#!isocline 3\n" p)
  (display "(+ 1 2)\n" p)
  (display "(display \"hello\")\\n(next line)\n" p)
  (display "foo\\\\bar\n" p)
  (close-output-port p)
  (let ((res (%repl-history-file-lines tmp)))
    ;; count 为条目数（不含 #! 头）
    (check (car res) => 3)
    (check (length (cdr res)) => 3)
    (check (car (cdr res)) => "(+ 1 2)")
    ;; 转义保持原文（未反转义）
    (check (cadr (cdr res)) => "(display \"hello\")\\n(next line)")
    (check (caddr (cdr res)) => "foo\\\\bar")
  ) ;let
  (remove tmp)
) ;let*

;; 文件不存在
(let ((res (%repl-history-file-lines (string-append (os-temp-dir) "/goldrepl-no-such-file-xyz.txt")
           ) ;%repl-history-file-lines
      ) ;res
     ) ;
  (check (car res) => 0)
  (check (cdr res) => '())
) ;let

;; 无 #! 头部时逐条计数
(let* ((tmp (string-append (os-temp-dir)
              "/goldrepl-history-test2-"
              (number->string (getpid))
              ".txt"
            ) ;string-append
       ) ;tmp
       (p (open-output-file tmp))
      ) ;
  (display "(+ 1 2)\n" p)
  (display "abc\n" p)
  (close-output-port p)
  (let ((res (%repl-history-file-lines tmp)))
    (check (car res) => 2)
    (check (length (cdr res)) => 2)
  ) ;let
  (remove tmp)
) ;let*

(check-report)
