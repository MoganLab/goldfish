;; parse-keys
;; 解析连续按键流，将多个按键的输入字符串转换为按键名列表。
;;
;; 语法
;; ----
;; (parse-keys input)
;;
;; 参数
;; ----
;; input : string?，待解析的按键输入流
;;
;; 返回值
;; ----
;; 返回按键名列表，普通字符为字符串、控制键与转义序列为按键符号，
;; 空输入返回空列表。

(import (liii check) (liii tui keys))

(check-set-mode! 'report-failed)

;; 空输入
(check (parse-keys "") => '())

;; 纯普通 ASCII 文本流
(check (parse-keys "abc") => '("a" "b" "c"))
(check (parse-keys "hello world")
  =>
  '("h" "e" "l" "l" "o" " " "w" "o" "r" "l" "d")
) ;check

;; UTF-8 多字节文本流
(check (parse-keys "你好") => '("你" "好"))
(check (parse-keys "金鱼 Scheme")
  =>
  '("金" "鱼" " " "S" "c" "h" "e" "m" "e")
) ;check

;; 控制字符流
(check (parse-keys "\r\n") => '(enter))
(check (parse-keys "\r\n\t") => '(enter tab))
(check (parse-keys "\b") => '(backspace backspace))
(check (parse-keys "\x03;\x04;") => '(ctrl-c ctrl-d))

;; 方向键与转义序列流
(check (parse-keys "\x1b;[A\x1b;[B\x1b;[C\x1b;[D") => '(up down right left))
(check (parse-keys "\x1b;OA\x1b;OB") => '(up down))

;; 混合输入（文本、方向键、回车、快捷键）
(check (parse-keys "w\x1b;[As\x1b;[B") => '("w" up "s" down))
(check (parse-keys "quit\r\n") => '("q" "u" "i" "t" enter))
(check (parse-keys "echo \x1b;[1;5A\t\x1b;a\x03;")
  =>
  '("e" "c" "h" "o" " " ctrl-up tab alt-a ctrl-c)
) ;check

;; Meta 前缀连续按键流与不粘连断言
(check (parse-keys "\x1b;\x1b;[A") => '(alt-up))
(check (parse-keys "a\x1b;\x1b;[Ab") => '("a" alt-up "b"))
(check (parse-keys "\x1b;\x1b;[A\x1b;[A") => '(alt-up up))
(check (parse-keys "\x1b;\x1b;[57352;3u") => '(alt-up))

;; 错误处理
(check-catch 'type-error (parse-keys 'not-a-string))
(check-catch 'type-error (parse-keys #f))
(check-catch 'type-error (parse-keys 42))

(check-report)
