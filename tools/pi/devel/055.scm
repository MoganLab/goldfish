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
  (liii tui text-width)
  (liii tui component)
) ;import

(check-set-mode! 'report-failed)

;; [055] 验收脚本：(liii tui text-width) 显示宽度与 ANSI 感知文本工具
;; 逐条对应 tools/pi/devel/055.md 验收点：
;;   1) 门禁检查：构建 / fmt / git diff --exit-code / check / doc
;;   2) 单文件行数 <= 300 行硬约束（代码与测试文件）
;;   3) 单函数单元测试与回归测试全量通过（CLI 执行）
;;   4) strip-ansi 纯函数行为与异常拦截
;;   5) display-width 视觉显示列宽精确计算
;;   6) truncate-to-width 安全截断、双宽防劈裂与 ANSI 闭合
;;   7) wrap-text 显示宽度折行、双宽处理与 ANSI 样式跨行
;;   8) 054 基础组件平滑升级至显示宽度语义

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-055-" tag ".txt")))
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

(display "=== 验收点 2: 任务 055 相关代码与测试文件行数硬约束 (<= 300 行) ===\n"
) ;display
(let ((files '("goldfish/liii/tui.scm" "goldfish/liii/tui/component.scm"
               "goldfish/liii/tui/text-width.scm"
               "goldfish/liii/tui/text-width/core.scm"
               "goldfish/liii/tui/text-width/truncate.scm"
               "goldfish/liii/tui/text-width/wrap.scm" "tests/liii/tui-test.scm"
               "tests/liii/tui/text-width/strip-ansi-test.scm"
               "tests/liii/tui/text-width/display-width-test.scm"
               "tests/liii/tui/text-width/truncate-to-width-test.scm"
               "tests/liii/tui/text-width/wrap-text-test.scm"
               "tests/liii/tui/component/make-text-test.scm"
               "tests/liii/tui/component/make-truncated-text-test.scm"
               "tools/pi/devel/055.scm")
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
(check (%run-ok? "t-strip-ansi"
         "timeout 120 bin/gf tests/liii/tui/text-width/strip-ansi-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-disp-w"
         "timeout 120 bin/gf tests/liii/tui/text-width/display-width-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-trunc-w"
         "timeout 120 bin/gf tests/liii/tui/text-width/truncate-to-width-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-wrap-t"
         "timeout 120 bin/gf tests/liii/tui/text-width/wrap-text-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-tui" "timeout 120 bin/gf tests/liii/tui-test.scm" 120) => 0)
(check (%run-ok? "t-make-text"
         "timeout 120 bin/gf tests/liii/tui/component/make-text-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-make-trunc"
         "timeout 120 bin/gf tests/liii/tui/component/make-truncated-text-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-comp-p"
         "timeout 120 bin/gf tests/liii/tui/component/component-p-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-make-comp"
         "timeout 120 bin/gf tests/liii/tui/component/make-component-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check

(display "=== 验收点 4: strip-ansi 纯函数行为与异常拦截 ===\n")
(check (strip-ansi "") => "")
(check (strip-ansi "plain text 123") => "plain text 123")
(check (strip-ansi "\x1b;[31;1mred bold\x1b;[0m") => "red bold")
(check (strip-ansi "\x1b;[38;5;208morange\x1b;[39m") => "orange")
(check (strip-ansi "\x1b;[38;2;10;20;30mrgb\x1b;[0m") => "rgb")
(check (strip-ansi "\x1b;[2J\x1b;[H\x1b;[?25hclear") => "clear")
(check (strip-ansi "\x1b;[32m金鱼\x1b;[0m Scheme \x1b;[1m终端\x1b;[0m")
  =>
  "金鱼 Scheme 终端"
) ;check
(check-catch 'type-error (strip-ansi 123))
(check-catch 'type-error (strip-ansi #f))

(display "=== 验收点 5: display-width 视觉显示列宽计算 ===\n")
(check (display-width "") => 0)
(check (display-width "abc") => 3)
(check (display-width "hello, world!") => 13)
(check (display-width "\x1b;[31mred\x1b;[0m") => 3)
(check (display-width "\x1b;[1;32;40m\x1b;[4mstyled\x1b;[0m") => 6)
(check (display-width "你") => 2)
(check (display-width "金鱼Scheme") => 10)
(check (display-width "【金鱼】！") => 10)
(check (display-width "😊") => 2)
(check (display-width "🚀") => 2)
(check (display-width "🚀金鱼😊Scheme") => 14)
(check (display-width "\x1b;[33m金鱼\x1b;[0m🚀") => 6)
(check (display-width (utf8->string #u8(226 128 139))) => 0)
(check-catch 'type-error (display-width 'not-string))
(check-catch 'type-error (display-width #t))

(display "=== 验收点 6: truncate-to-width 安全截断、双宽防劈裂与 ANSI 闭合 ===\n"
) ;display
(check (truncate-to-width "" 10) => "")
(check (truncate-to-width "hello" 0) => "")
(check (truncate-to-width "hello" -3) => "")
(check (truncate-to-width "hello" 5) => "hello")
(check (truncate-to-width "hello" 10) => "hello")
(check (truncate-to-width "hello world" 8) => "hello...")
(check (truncate-to-width "hello world" 6 "-") => "hello-")
(check (truncate-to-width "hello world" 5 "") => "hello")
(check (truncate-to-width "hello world" 2 "...") => "..")
(check (truncate-to-width "hello world" 1 "...") => ".")
;; 双宽 CJK 防劈裂：7 列放 "你好..." (4+3=7)，6 列放不下第二个汉字只放 "你..." (2+3=5 <= 6)
(check (truncate-to-width "你好世界" 7) => "你好...")
(check (truncate-to-width "你好世界" 6) => "你...")
;; ANSI 样式安全保护与尾部闭合
(check (truncate-to-width "\x1b;[31mhello world\x1b;[0m" 8)
  =>
  "\x1b;[31mhello...\x1b;[0m"
) ;check
(check (truncate-to-width "\x1b;[32m你好世界\x1b;[0m" 7)
  =>
  "\x1b;[32m你好...\x1b;[0m"
) ;check
(check-catch 'type-error (truncate-to-width 100 5))
(check-catch 'type-error (truncate-to-width "abc" "5"))
(check-catch 'type-error (truncate-to-width "abc" 5 'ellipsis))

(display "=== 验收点 7: wrap-text 显示宽度折行、双宽处理与 ANSI 跨行 ===\n"
) ;display
(check (wrap-text "" 10) => '(""))
(check (wrap-text "hello" 0) => '(""))
(check (wrap-text "hello" -1) => '(""))
(check (wrap-text "hello" 10) => '("hello"))
(check (wrap-text "hello world test" 11) => '("hello world" "test"))
(check (wrap-text "supercalifragilistic" 5)
  =>
  '("super" "calif" "ragil" "istic")
) ;check
(check (wrap-text "line1\nline2\nline3" 10) => '("line1" "line2" "line3"))
(check (wrap-text "你好世界" 4) => '("你好" "世界"))
(check (wrap-text "你好世界" 5) => '("你好" "世界"))
(check (wrap-text "金鱼Scheme终端用户界面" 10)
  =>
  '("金鱼Scheme" "终端用户界" "面")
) ;check
(check (wrap-text "\x1b;[31mhello world\x1b;[0m" 5)
  =>
  '("\x1b;[31mhello\x1b;[0m" "\x1b;[31mworld\x1b;[0m")
) ;check
(check-catch 'type-error (wrap-text 123 10))
(check-catch 'type-error (wrap-text "str" #f))

(display "=== 验收点 8: 054 基础组件平滑升级至显示宽度语义 ===\n"
) ;display
(let ((t (make-text "金鱼Scheme终端用户界面")))
  (check-true (component? t))
  (check (component-render t 10) => '("金鱼Scheme" "终端用户界" "面"))
) ;let
(let ((t-ansi (make-text "\x1b;[34mhello world\x1b;[0m")))
  (check (component-render t-ansi 5)
    =>
    '("\x1b;[34mhello\x1b;[0m" "\x1b;[34mworld\x1b;[0m")
  ) ;check
) ;let
(let ((tt (make-truncated-text "你好世界")))
  (check-true (component? tt))
  (check (component-render tt 7) => '("你好..."))
  (check (component-render tt 6) => '("你..."))
) ;let
(let ((tt-ansi (make-truncated-text "\x1b;[35mhello world\x1b;[0m")))
  (check (component-render tt-ansi 8) => '("\x1b;[35mhello...\x1b;[0m"))
) ;let
(let ((sp (make-spacer 2)))
  (check (component-render sp 20) => '("" ""))
) ;let

(check-report)
