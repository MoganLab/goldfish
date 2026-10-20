(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi bash)
  (liii tui)
  (liii tui keys)
  (liii tui keys parser)
  (liii tui keys table)
) ;import

(check-set-mode! 'report-failed)

;; [056] 验收脚本：(liii tui keys) 键盘输入与转义序列解析
;; 逐条对应 tools/pi/devel/056.md 验收点：
;;   1) 门禁检查：构建 / fmt / git diff --exit-code / check / doc
;;   2) 单文件行数 <= 300 行硬约束（代码与测试文件）
;;   3) 单函数单元测试与回归测试全量通过（CLI 执行）
;;   4) ASCII 字符、多字节 UTF-8、回车退格与孤立 ESC 解析
;;   5) Ctrl 控制组合键（ctrl-a 到 ctrl-z 及符号组合）
;;   6) ANSI / VT100 / xterm 方向键与导航键（up/down/left/right, home/end 等）
;;   7) 功能键（F1-F12）与带修饰键扩展序列（Shift/Ctrl/Alt 组合）
;;   8) parse-key 带剩余字符串模式与类型检查拦截
;;   9) parse-keys 批量流式解析、连击与游戏输入模拟

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-056-" tag ".txt")))
    (let ((ret (%run-shell (string-append cmd " > " tmp-out " 2>&1") timeout)))
      (let ((out
              (if (and (file-exists? tmp-out) (path-file? (path tmp-out)))
                (path-read-text tmp-out)
                ""
              ) ;if
            ) ;out
           ) ;
        (when (file-exists? tmp-out)
          (delete-file tmp-out)
        ) ;when
        (cons ret out)
      ) ;let
    ) ;let
  ) ;let
) ;define

(define (%run-ok? tag cmd timeout)
  (car (%run-capture tag cmd timeout))
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / diff / check / doc） ===\n"
) ;display
(check (%run-ok? "build" "timeout 300 xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt" "timeout 120 bin/gf fmt" 120) => 0)
(check (%run-ok? "diff" "git diff --exit-code goldfish/ tests/" 120) => 0)
(check (%run-ok? "check" "timeout 120 bin/gf check" 120) => 0)
(check (%run-ok? "doc" "timeout 120 bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 056 相关代码与测试文件行数硬约束 (<= 300 行) ===\n"
) ;display
(let ((files '("goldfish/liii/tui.scm" "goldfish/liii/tui/keys.scm"
               "goldfish/liii/tui/keys/table.scm"
               "goldfish/liii/tui/keys/parser.scm" "tests/liii/tui-test.scm"
               "tests/liii/tui/keys/parse-key-test.scm"
               "tests/liii/tui/keys/parse-keys-test.scm"
               "tools/pi/devel/056.scm")
      ) ;files
     ) ;
  (for-each
    (lambda (f)
      (let ((line-count (length (string-split (path-read-text f) "\n"))))
        (check-true (<= line-count 300))
      ) ;let
    ) ;lambda
    files
  ) ;for-each
) ;let

(display "=== 验收点 3: 单函数单元测试与回归测试 CLI 执行验证 ===\n"
) ;display
(check (%run-ok? "t-parse-key"
         "timeout 120 bin/gf tests/liii/tui/keys/parse-key-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-parse-keys"
         "timeout 120 bin/gf tests/liii/tui/keys/parse-keys-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-tui" "timeout 120 bin/gf tests/liii/tui-test.scm" 120) => 0)
(check (%run-ok? "t-text-width"
         "timeout 120 bin/gf tests/liii/tui/text-width/display-width-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check

(display "=== 验收点 4: ASCII 字符、多字节 UTF-8、回车退格与孤立 ESC ===\n"
) ;display
(check (parse-key "a") => "a")
(check (parse-key "Z") => "Z")
(check (parse-key "9") => "9")
(check (parse-key " ") => " ")
(check (parse-key "+") => "+")
(check (parse-key "金") => "金")
(check (parse-key "鱼") => "鱼")
(check (parse-key "🐍") => "🐍")
(check (parse-key "😊") => "😊")
(check (parse-key "\r") => 'enter)
(check (parse-key "\n") => 'enter)
(check (parse-key "\r\n") => 'enter)
(check (parse-key "\t") => 'tab)
(check (parse-key "\b") => 'backspace)
(check (parse-key "") => 'backspace)
(check (parse-key "\x1b;") => 'escape)

(display "=== 验收点 5: Ctrl 控制组合键 ===\n")
(check (parse-key "\x00;") => 'ctrl-space)
(check (parse-key "\x01;") => 'ctrl-a)
(check (parse-key "\x02;") => 'ctrl-b)
(check (parse-key "\x03;") => 'ctrl-c)
(check (parse-key "\x04;") => 'ctrl-d)
(check (parse-key "\x05;") => 'ctrl-e)
(check (parse-key "\x0b;") => 'ctrl-k)
(check (parse-key "\f") => 'ctrl-l)
(check (parse-key "\x1a;") => 'ctrl-z)
(check (parse-key "\x1c;") => 'ctrl-\)
(check (parse-key "\x1d;") => 'ctrl-])
(check (parse-key "\x1e;") => 'ctrl-^)
(check (parse-key "\x1f;") => 'ctrl-_)

(display "=== 验收点 6: ANSI / VT100 / xterm 方向键与导航键 ===\n")
(check (parse-key "\x1b;[A") => 'up)
(check (parse-key "\x1b;[B") => 'down)
(check (parse-key "\x1b;[C") => 'right)
(check (parse-key "\x1b;[D") => 'left)
(check (parse-key "\x1b;OA") => 'up)
(check (parse-key "\x1b;OB") => 'down)
(check (parse-key "\x1b;OC") => 'right)
(check (parse-key "\x1b;OD") => 'left)
(check (parse-key "\x1b;[H") => 'home)
(check (parse-key "\x1b;[F") => 'end)
(check (parse-key "\x1b;OH") => 'home)
(check (parse-key "\x1b;OF") => 'end)
(check (parse-key "\x1b;[1~") => 'home)
(check (parse-key "\x1b;[4~") => 'end)
(check (parse-key "\x1b;[5~") => 'page-up)
(check (parse-key "\x1b;[6~") => 'page-down)
(check (parse-key "\x1b;[2~") => 'insert)
(check (parse-key "\x1b;[3~") => 'delete)

(display "=== 验收点 7: 功能键（F1-F12）与带修饰键扩展序列 ===\n"
) ;display
(check (parse-key "\x1b;OP") => 'f1)
(check (parse-key "\x1b;OQ") => 'f2)
(check (parse-key "\x1b;OR") => 'f3)
(check (parse-key "\x1b;OS") => 'f4)
(check (parse-key "\x1b;[[A") => 'f1)
(check (parse-key "\x1b;[[B") => 'f2)
(check (parse-key "\x1b;[[C") => 'f3)
(check (parse-key "\x1b;[[D") => 'f4)
(check (parse-key "\x1b;[15~") => 'f5)
(check (parse-key "\x1b;[17~") => 'f6)
(check (parse-key "\x1b;[18~") => 'f7)
(check (parse-key "\x1b;[19~") => 'f8)
(check (parse-key "\x1b;[20~") => 'f9)
(check (parse-key "\x1b;[21~") => 'f10)
(check (parse-key "\x1b;[23~") => 'f11)
(check (parse-key "\x1b;[24~") => 'f12)
(check (parse-key "\x1b;[1;2A") => 'shift-up)
(check (parse-key "\x1b;[1;3B") => 'alt-down)
(check (parse-key "\x1b;[1;5C") => 'ctrl-right)
(check (parse-key "\x1b;[1;6D") => 'shift-ctrl-left)
(check (parse-key "\x1b;[Z") => 'shift-tab)
(check (parse-key "\x1b;a") => 'alt-a)
(check (parse-key "\x1b;\r") => 'alt-enter)
(check (parse-key "\x1b;\t") => 'alt-tab)
(check (parse-key "\x1b;") => 'alt-backspace)

(display "=== 验收点 8: parse-key 带剩余字符串模式与类型检查 ===\n"
) ;display
(check (parse-key "") => #f)
(check (parse-key "" #t) => '(#f . ""))
(check (parse-key "abc" #t) => '("a" . "bc"))
(check (parse-key "\x1b;[Arest" #t) => '(up . "rest"))
(check (parse-key "\r\nhello" #t) => '(enter . "hello"))
(check (parse-key "金鱼" #t) => '("金" . "鱼"))
(check (parse-key "\x1b;[1;5Afoo" #t) => '(ctrl-up . "foo"))
(check-catch 'type-error (parse-key 123))
(check-catch 'type-error (parse-key #f))

(display "=== 验收点 9: parse-keys 批量流式解析、连击与游戏输入模拟 ===\n"
) ;display
(check (parse-keys "") => '())
(check (parse-keys "wasd") => '("w" "a" "s" "d"))
(check (parse-keys "\x1b;[A\x1b;[B\x1b;[C\x1b;[D") => '(up down right left))
(check (parse-keys "w\x1b;[As\x1b;[Bd") => '("w" up "s" down "d"))
(check (parse-keys "hello\r\nworld\x04;")
  =>
  '("h" "e" "l" "l" "o" enter "w" "o" "r" "l" "d" ctrl-d)
) ;check
(check (parse-keys "snake\x1b;[A\x1b;[Cq")
  =>
  '("s" "n" "a" "k" "e" up right "q")
) ;check
(check (parse-keys "贪吃蛇🐍") => '("贪" "吃" "蛇" "🐍"))
(check (parse-keys "\x1b;[3~x\x1b;[2~") => '(delete "x" insert))
(check-catch 'type-error (parse-keys 456))
(check-catch 'type-error (parse-keys #t))

(check-report)
