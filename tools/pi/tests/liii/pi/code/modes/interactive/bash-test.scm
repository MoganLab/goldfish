(import (scheme base)
  (liii check)
  (liii string)
  (liii list)
  (liii unicode)
  (liii go)
  (liii time)
  (liii tui)
  (liii pi code modes interactive bash)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report)

;; 构造 1..n 的多行输出文本
(define (%nums-text n)
  (string-join (map number->string (iota n 1)) "\n")
) ;define

;; ============================================================
;; 一、bash 卡片组件渲染（对齐 pi BashExecutionComponent）
;; ============================================================

;; 1. 运行中卡片：绿色上下边框 + "$ cmd" 头 + Running... 提示
(let* ((card (make-bash-card "ls"))
       (lines (component-render card 80))
       (plain (map strip-ansi lines))
      ) ;
  ;; 边框 + 头 + Running + 边框 + 尾部空行 = 5 行
  (check (length lines) => 5)
  (check (car plain) => (utf8-make-string 80 #\─))
  (check-true (string-contains? (car lines) "38;2;94;178;134"))
  (check (list-ref plain 1) => " $ ls")
  (check-true (string-contains? (list-ref plain 2) "Running"))
  (check-true (string-contains? (list-ref lines 1) "38;2;94;178;134"))
  (check (list-ref plain 3) => (utf8-make-string 80 #\─))
  (check (list-ref plain 4) => "")
) ;let*

;; 2. 折叠渲染：35 行输出只显示最后 20 行 + "... 15 more lines (ctrl+o to expand)"
(let* ((card (make-bash-card "seq 1 35"))
       (_ (bash-card-set-output! card (%nums-text 35)))
       (_2 (bash-card-set-complete! card 0))
       (lines (component-render card 80))
       (plain (map strip-ansi lines))
      ) ;
  ;; 边框 + 头 + 空 + 20 输出 + 空 + 状态 + 边框 + 空 = 27 行
  (check (length lines) => 27)
  (check (list-ref plain 1) => " $ seq 1 35")
  (check (list-ref plain 3) => " 16")
  (check (list-ref plain 22) => " 35")
  (check-true (string-contains? (list-ref plain 24) "... 15 more lines (ctrl+o to expand)"))
  (check (list-ref plain 25) => (utf8-make-string 80 #\─))
  ;; 输出行使用暗灰前景色（muted）
  (check-true (string-contains? (list-ref lines 3) "38;2;157;165;169"))
) ;let*

;; 3. 展开渲染：显示全部 35 行 + "(ctrl+o to collapse)"
(let* ((card (make-bash-card "seq 1 35"))
       (_ (bash-card-set-output! card (%nums-text 35)))
       (_2 (bash-card-set-complete! card 0))
       (_3 (bash-card-set-expanded! card #t))
       (lines (component-render card 80))
       (plain (map strip-ansi lines))
      ) ;
  ;; 边框 + 头 + 空 + 35 输出 + 空 + 状态 + 边框 + 空 = 42 行
  (check (length lines) => 42)
  (check (list-ref plain 3) => " 1")
  (check (list-ref plain 37) => " 35")
  (check-true (string-contains? (list-ref plain 39) "(ctrl+o to collapse)"))
) ;let*

;; 4. 展开状态切换：set-expanded! 两次往返
(let* ((card (make-bash-card "seq 1 30"))
       (_ (bash-card-set-output! card (%nums-text 30)))
       (_2 (bash-card-set-complete! card 0))
      ) ;
  (bash-card-set-expanded! card #t)
  (check-true (bash-card-expanded? card))
  (bash-card-set-expanded! card #f)
  (check-false (bash-card-expanded? card))
  (let ((plain (map strip-ansi (component-render card 80))))
    ;; 折叠态恢复 20 行预览：26 = 30 - 20 + 1 为首行
    (check (list-ref plain 3) => " 11")
    (check-true (string-contains? (list-ref plain 24) "... 10 more lines (ctrl+o to expand)"))
  ) ;let
) ;let*

;; 5. 错误退出：非零退出码在状态区显示 "(exit N)"（红色）
(let* ((card (make-bash-card "false"))
       (_ (bash-card-set-output! card "boom"))
       (_2 (bash-card-set-complete! card 1))
       (lines (component-render card 80))
       (plain (map strip-ansi lines))
      ) ;
  ;; 边框 + 头 + 空 + 1 输出 + 空 + (exit 1) + 边框 + 空 = 8 行
  (check (length lines) => 8)
  (check (list-ref plain 3) => " boom")
  (check-true (string-contains? (list-ref plain 5) "(exit 1)"))
  (check-true (string-contains? (list-ref lines 5) "38;2;234;127;129"))
) ;let*

;; 6. 输出不足 20 行时不显示省略提示
(let* ((card (make-bash-card "echo hi"))
       (_ (bash-card-set-output! card "hi"))
       (_2 (bash-card-set-complete! card 0))
       (plain (map strip-ansi (component-render card 80)))
      ) ;
  ;; 边框 + 头 + 空 + 1 输出 + 边框 + 空 = 6 行（无状态行）
  (check (length plain) => 6)
  (check-true (string-contains? (list-ref plain 3) "hi"))
) ;let*

;; 7. 空输出且成功：仅头与边框
(let* ((card (make-bash-card "true"))
       (_ (bash-card-set-output! card ""))
       (_2 (bash-card-set-complete! card 0))
       (plain (map strip-ansi (component-render card 80)))
      ) ;
  ;; 边框 + 头 + 边框 + 空 = 4 行
  (check (length plain) => 4)
) ;let*

;; 7b. 输出含 ANSI 转义序列：set-output! 剥离后再拆行，渲染无残留转义
(let* ((card (make-bash-card "ls --color"))
       (_ (bash-card-set-output! card "\x1b;[31mred\x1b;[0m\n\x1b;[1;32mbold green\x1b;[0m"))
       (_2 (bash-card-set-complete! card 0))
       (lines (component-render card 80))
       (plain (map strip-ansi lines))
      ) ;
  ;; 剥离后输出行为纯文本
  (check (list-ref plain 3) => " red")
  (check (list-ref plain 4) => " bold green")
  ;; 渲染行中除卡片自身颜色码外，不应残留输出自带的 SGR（31m / 1;32m）
  (check-false (string-contains? (list-ref lines 3) "[31m"))
  (check-false (string-contains? (list-ref lines 4) "1;32m"))
) ;let*

;; 7c. 输出含 \r（进度条、CRLF 行尾）：对齐上游 appendOutput，
;; \r\n→\n、\r→\n（进度条帧变为独立行），尾部空行不渲染
(let* ((card (make-bash-card "cmd-with-progress"))
       (_ (bash-card-set-output! card "step1\rstep2\rstep3\ndone\r\n"))
       (_2 (bash-card-set-complete! card 0))
       (plain (map strip-ansi (component-render card 80)))
      ) ;
  ;; 边框+头+空+4 输出+边框+空 = 9 行，无残留 \r，帧成为独立行
  (check (length plain) => 9)
  (check-false (string-contains? (string-join plain "\n") "\r"))
  (check (list-ref plain 3) => " step1")
  (check (list-ref plain 4) => " step2")
  (check (list-ref plain 5) => " step3")
  (check (list-ref plain 6) => " done")
) ;let*

;; 7d. TAB 分隔的列式输出（如 gh workflow list）：展开为定宽 3 空格，
;; 与 display-width 一致，卡片背景不错位
(let* ((card (make-bash-card "gh workflow list"))
       (_ (bash-card-set-output! card "CI on Debian\tactive\t110808756"))
       (_2 (bash-card-set-complete! card 0))
       (plain (map strip-ansi (component-render card 80)))
      ) ;
  (check (list-ref plain 3) => " CI on Debian   active   110808756")
  ;; 渲染行宽度（含卡片自身颜色码被 strip 后）与终端实际列宽一致，无 TAB 残留
  (check-false (string-contains? (list-ref plain 3) "\t"))
) ;let*

;; 7e. OSC 转义序列（终端标题、背景色查询）被剥离
(let* ((card (make-bash-card "gh auth status"))
       (_ (bash-card-set-output! card "\x1b]11;?\x1b\\\x1b[32mok\x1b[0m"))
       (_2 (bash-card-set-complete! card 0))
       (plain (map strip-ansi (component-render card 80)))
      ) ;
  (check (list-ref plain 3) => " ok")
  (check-false (string-contains? (string-join plain "\n") "]11;"))
) ;let*

(check-report)
