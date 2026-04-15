(import (liii check))
(import (scheme file))
(import (scheme base))
;; 测试 read-line 基本功能
;; read-line 函数从输入端口读取一行文本，返回不包含换行符的字符串
;; 当到达文件末尾时，返回 eof 对象
;; 可选的 with-eol 参数为 #t 时，返回的字符串包含行末的换行符
(check-set-mode! 'report-failed)
;; 测试1: 从字符串端口读取多行
(let ((port (open-input-string "line1\nline2\nline3"
            ) ;open-input-string
      ) ;port
     ) ;
  (check (read-line port) => "line1")
  (check (read-line port) => "line2")
  (check (read-line port) => "line3")
  (check (eof-object? (read-line port))
    =>
    #t
  ) ;check
) ;let
;; 测试2: 从字符串端口读取行（包含换行符）
(let ((port (open-input-string "line1\nline2\n")
      ) ;port
     ) ;
  (check (read-line port #t) => "line1\n")
  (check (read-line port #t) => "line2\n")
  ;; 最后一个换行符后没有内容，返回 eof
  (check (eof-object? (read-line port #t))
    =>
    #t
  ) ;check
) ;let
;; 测试3: 只有一行的字符串（无换行符结尾）
(let ((port (open-input-string "single line"))
     ) ;
  (check (read-line port)
    =>
    "single line"
  ) ;check
  (check (eof-object? (read-line port))
    =>
    #t
  ) ;check
) ;let
;; 测试4: 空字符串
(let ((port (open-input-string "")))
  (check (eof-object? (read-line port))
    =>
    #t
  ) ;check
) ;let
;; 测试5: 只有换行符 - 返回空字符串，然后eof
(let ((port (open-input-string "\n")))
  (check (read-line port) => "")
  (check (eof-object? (read-line port))
    =>
    #t
  ) ;check
) ;let
;; 测试6: 多个连续的换行符
;; "a\n\nb\n" 的结构：a行 + 空行 + b行 + 换行符结束
(let ((port (open-input-string "a\n\nb\n")))
  (check (read-line port) => "a")
  (check (read-line port) => "")
  (check (read-line port) => "b")
  ;; 最后一个换行符后如果没有内容，直接返回eof
  (check (eof-object? (read-line port))
    =>
    #t
  ) ;check
) ;let
;; 测试7: 使用 #f 明确指定不包含换行符（默认行为）
(let ((port (open-input-string "hello\n")))
  (check (read-line port #f) => "hello")
  ;; 换行符后没有内容，返回 eof（不是空字符串）
  (check (eof-object? (read-line port #f))
    =>
    #t
  ) ;check
) ;let
;; 测试8: 多行字符串，最后一行有换行符
(let ((port (open-input-string "first\nsecond\n")
      ) ;port
     ) ;
  (check (read-line port) => "first")
  (check (read-line port) => "second")
  ;; 最后一个换行符后没有内容，返回 eof
  (check (eof-object? (read-line port))
    =>
    #t
  ) ;check
) ;let
;; 测试9: 带有空格的文本行
(let ((port (open-input-string "  leading spaces\ntrailing spaces  \n"
            ) ;open-input-string
      ) ;port
     ) ;
  (check (read-line port)
    =>
    "  leading spaces"
  ) ;check
  (check (read-line port)
    =>
    "trailing spaces  "
  ) ;check
  (check (eof-object? (read-line port))
    =>
    #t
  ) ;check
) ;let
;; 测试10: 空行在多行文本中
(let ((port (open-input-string "start\n\n\nend")
      ) ;port
     ) ;
  (check (read-line port) => "start")
  (check (read-line port) => "")
  (check (read-line port) => "")
  (check (read-line port) => "end")
  (check (eof-object? (read-line port))
    =>
    #t
  ) ;check
) ;let
(check-report)