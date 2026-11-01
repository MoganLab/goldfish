;; read-line
;; 从输入端口读取一行文本，返回不包含换行符（或包含换行符）的字符串。
;;
;;
;; 语法
;; ----
;; (read-line)
;; (read-line port)
;; (read-line port with-eol)
;;
;; 参数
;; ----
;; port : input-port? (可选)
;; 输入端口，默认为当前输入端口。
;;
;; with-eol : boolean? (可选)
;; 是否保留行末换行符，默认为 #f。若为 #t，返回的字符串包含换行符。
;;
;; 返回值
;; ----
;; string? 或 eof-object?
;; 读取到的行字符串。如果已到达文件末尾且未读取到任何字符，则返回 EOF 对象。
;;
;; 说明
;; ----
;; 1. read-line 函数从输入端口读取一行文本，默认返回不包含换行符的字符串
;; 2. 当到达文件末尾时，返回 eof 对象
;; 3. 可选的 with-eol 参数为 #t 时，返回的字符串包含行末的换行符

(import (liii check))
(import (scheme file))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 测试1: 从字符串端口读取多行
(let ((port (open-input-string "line1\nline2\nline3")))
  (check (read-line port) => "line1")
  (check (read-line port) => "line2")
  (check (read-line port) => "line3")
  (check (eof-object? (read-line port)) => #t)
) ;let
;; 测试2: 从字符串端口读取行（包含换行符）
(let ((port (open-input-string "line1\nline2\n")))
  (check (read-line port #t) => "line1\n")
  (check (read-line port #t) => "line2\n")
  ;; 最后一个换行符后没有内容，返回 eof
  (check (eof-object? (read-line port #t)) => #t)
) ;let
;; 测试3: 只有一行的字符串（无换行符结尾）
(let ((port (open-input-string "single line")))
  (check (read-line port) => "single line")
  (check (eof-object? (read-line port)) => #t)
) ;let
;; 测试4: 空字符串
(let ((port (open-input-string "")))
  (check (eof-object? (read-line port)) => #t)
) ;let
;; 测试5: 只有换行符 - 返回空字符串，然后eof
(let ((port (open-input-string "\n")))
  (check (read-line port) => "")
  (check (eof-object? (read-line port)) => #t)
) ;let
;; 测试6: 多个连续的换行符
;; "a\n\nb\n" 的结构：a行 + 空行 + b行 + 换行符结束
(let ((port (open-input-string "a\n\nb\n")))
  (check (read-line port) => "a")
  (check (read-line port) => "")
  (check (read-line port) => "b")
  ;; 最后一个换行符后如果没有内容，直接返回eof
  (check (eof-object? (read-line port)) => #t)
) ;let
;; 测试7: 使用 #f 明确指定不包含换行符（默认行为）
(let ((port (open-input-string "hello\n")))
  (check (read-line port #f) => "hello")
  ;; 换行符后没有内容，返回 eof（不是空字符串）
  (check (eof-object? (read-line port #f)) => #t)
) ;let
;; 测试8: 多行字符串，最后一行有换行符
(let ((port (open-input-string "first\nsecond\n")))
  (check (read-line port) => "first")
  (check (read-line port) => "second")
  ;; 最后一个换行符后没有内容，返回 eof
  (check (eof-object? (read-line port)) => #t)
) ;let
;; 测试9: 带有空格的文本行
(let ((port (open-input-string "  leading spaces\ntrailing spaces  \n")))
  (check (read-line port) => "  leading spaces")
  (check (read-line port) => "trailing spaces  ")
  (check (eof-object? (read-line port)) => #t)
) ;let
;; 测试10: 空行在多行文本中
(let ((port (open-input-string "start\n\n\nend")))
  (check (read-line port) => "start")
  (check (read-line port) => "")
  (check (read-line port) => "")
  (check (read-line port) => "end")
  (check (eof-object? (read-line port)) => #t)
) ;let

;; 测试11: read-line 默认从当前输入端口读取
(let ((orig (current-input-port))
      (p (open-input-string "hello default\nworld default\n"))
     ) ;
  (set-current-input-port p)
  (check (read-line) => "hello default")
  (check (read-line) => "world default")
  (check (eof-object? (read-line)) => #t)
  (set-current-input-port orig)
) ;let

;; 测试12: read-line 从真实标准输入端口读取（默认去除换行符，with-eol 保留换行符）
(import (liii subprocess) (liii either) (liii string))
(let* ((res (run-either (list "bin/gf"
                          "-e"
                          "(let* ((l1 (read-line)) (l2 (read-line (current-input-port) #t)) (l3 (read-line))) (list l1 l2 l3))"
                        ) ;list
              :input  "line 1\nline 2\nline 3\n"
              :stdout 'capture
            ) ;run-either
       ) ;res
      ) ;
  (check-true (either-right? res))
  (check (string-replace (to-right res) "\r\n" "\n")
    =>
    "(\"line 1\" \"line 2\\n\" \"line 3\")\n"
  ) ;check
) ;let*

(check-report)
