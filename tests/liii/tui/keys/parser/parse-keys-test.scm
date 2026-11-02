(import (liii check)
        (liii tui keys))

(check-set-mode! 'report-failed)

;; 空输入
(check (parse-keys "") => '())

;; 纯普通 ASCII 文本流
(check (parse-keys "abc") => '("a" "b" "c"))
(check (parse-keys "hello world")
       => '("h" "e" "l" "l" "o" " " "w" "o" "r" "l" "d"))

;; UTF-8 多字节文本流
(check (parse-keys "你好") => '("你" "好"))
(check (parse-keys "金鱼 Scheme")
       => '("金" "鱼" " " "S" "c" "h" "e" "m" "e"))

;; 控制字符流
(check (parse-keys "\r\n") => '(enter))
(check (parse-keys "\r\n\t") => '(enter tab))
(check (parse-keys "\x08\x7f") => '(backspace backspace))
(check (parse-keys "\x03\x04") => '(ctrl-c ctrl-d))

;; 方向键与转义序列流
(check (parse-keys "\x1b[A\x1b[B\x1b[C\x1b[D")
       => '(up down right left))
(check (parse-keys "\x1bOA\x1bOB")
       => '(up down))

;; 混合输入（文本、方向键、回车、快捷键）
(check (parse-keys "w\x1b[As\x1b[B")
       => '("w" up "s" down))
(check (parse-keys "quit\r\n")
       => '("q" "u" "i" "t" enter))
(check (parse-keys "echo \x1b[1;5A\t\x1b;a\x03")
       => '("e" "c" "h" "o" " " ctrl-up tab alt-a ctrl-c))

;; 错误处理
(check-catch 'type-error (parse-keys 'not-a-string))
(check-catch 'type-error (parse-keys #f))
(check-catch 'type-error (parse-keys 42))

(check-report)
