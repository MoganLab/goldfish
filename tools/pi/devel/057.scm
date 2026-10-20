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
  (liii tui terminal)
  (liii tui terminal protocol)
  (liii tui terminal size)
  (liii tui terminal virtual)
  (liii tui terminal virtual state)
  (liii tui terminal standard)
) ;import

(check-set-mode! 'report-failed)

;; [057] 验收脚本：(liii tui terminal) 终端抽象与 virtual-terminal
;; 逐条对应 tools/pi/devel/057.md 验收点：
;;   1) 门禁检查：构建 / fmt / check / doc
;;   2) 单文件行数 <= 300 行硬约束（代码与测试文件）
;;   3) 单函数单元测试与回归测试全量通过（CLI 执行）
;;   4) 终端尺寸探测纯函数与环境变量回退 (terminal-detect-size)
;;   5) virtual-terminal 基础创建、属性与光标初始化
;;   6) 文本写入、换行回车、向上滚动与 raw / screen 缓冲区快照
;;   7) 光标移动、边界安全约束、显隐控制与清屏 / 清行
;;   8) 动态调整尺寸 (virtual-terminal-resize!) 与行缓冲适配
;;   9) standard-terminal 真实终端抽象与 ANSI 转义序列输出
;;  10) (liii tui) 顶级门面与参数类型错误拦截

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-057-" tag ".txt")))
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

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "timeout 300 xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt" "timeout 120 bin/gf fmt" 120) => 0)
(check (%run-ok? "check" "timeout 120 bin/gf check" 120) => 0)
(check (%run-ok? "doc" "timeout 120 bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 057 相关代码与测试文件行数硬约束 (<= 300 行) ===\n"
) ;display
(let ((files '("goldfish/liii/tui.scm" "goldfish/liii/tui/terminal.scm"
               "goldfish/liii/tui/terminal/protocol.scm"
               "goldfish/liii/tui/terminal/size.scm"
               "goldfish/liii/tui/terminal/virtual/state.scm"
               "goldfish/liii/tui/terminal/virtual.scm"
               "goldfish/liii/tui/terminal/standard.scm"
               "tests/liii/tui-test.scm"
               "tests/liii/tui/terminal/terminal-detect-size-test.scm"
               "tests/liii/tui/terminal/make-virtual-terminal-test.scm"
               "tests/liii/tui/terminal/terminal-write-test.scm"
               "tests/liii/tui/terminal/terminal-cursor-test.scm"
               "tests/liii/tui/terminal/virtual-terminal-resize-test.scm"
               "tests/liii/tui/terminal/make-standard-terminal-test.scm"
               "tools/pi/devel/057.scm")
      ) ;files
     ) ;
  (for-each
    (lambda (f)
      (let* ((lines (string-split (path-read-text f) #\newline)) (n (length lines)))
        (check (<= n 300) => #t)
      ) ;let*
    ) ;lambda
    files
  ) ;for-each
) ;let

(display "=== 验收点 3: 单函数单元测试与回归测试全量通过 (CLI 执行) ===\n"
) ;display
(for-each
  (lambda (test-file)
    (check (%run-ok? "unit-test" (string-append "bin/gf " test-file) 30) => 0)
  ) ;lambda
  '("tests/liii/tui/terminal/terminal-detect-size-test.scm"
    "tests/liii/tui/terminal/make-virtual-terminal-test.scm"
    "tests/liii/tui/terminal/terminal-write-test.scm"
    "tests/liii/tui/terminal/terminal-cursor-test.scm"
    "tests/liii/tui/terminal/virtual-terminal-resize-test.scm"
    "tests/liii/tui/terminal/make-standard-terminal-test.scm"
    "tests/liii/tui-test.scm")
) ;for-each

(display "=== 验收点 4: 终端尺寸探测纯函数与环境变量回退 ===\n"
) ;display
(let ((mock-env
        (lambda (k)
          (cond ((string=? k "COLUMNS") "100")
                ((string=? k "LINES") "50")
                (else #f)
          ) ;cond
        ) ;lambda
      ) ;mock-env
     ) ;
  (check (terminal-detect-size mock-env) => '(100 50))
) ;let

(check (terminal-detect-size (lambda (k) "invalid")) => '(80 24))
(check (terminal-detect-size (lambda (k) #f)) => '(80 24))
(let ((sz (terminal-detect-size)))
  (check (and (pair? sz) (= (length sz) 2) (> (car sz) 0) (> (cadr sz) 0)) => #t)
) ;let

(display "=== 验收点 5: virtual-terminal 创建、属性与光标初始化 ===\n"
) ;display
(let ((vt (make-virtual-terminal :width 30 :height 10)))
  (check (terminal? vt) => #t)
  (check (virtual-terminal? vt) => #t)
  (check (terminal-size vt) => '(30 10))
  (check (terminal-cursor vt) => '(0 0))
  (check (terminal-cursor-visible? vt) => #t)
  (check (length (virtual-terminal-screen vt)) => 10)
  (check (car (virtual-terminal-screen vt)) => (make-string 30 #\space))
) ;let

(display "=== 验收点 6: 文本写入、换行回车、向上滚动与 raw / screen 缓冲区 ===\n"
) ;display
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (terminal-write vt "hello")
  (check (terminal-cursor vt) => '(5 0))
  (check (car (virtual-terminal-screen vt)) => "hello     ")
  (terminal-write vt "\nworld")
  (check (terminal-cursor vt) => '(5 1))
  (check (cadr (virtual-terminal-screen vt)) => "world     ")
  (terminal-write vt "\rabc")
  (check (terminal-cursor vt) => '(3 1))
  (check (cadr (virtual-terminal-screen vt)) => "abcld     ")
  ;; 滚动测试：向 3 行高的终端连续追加两行
  (terminal-write vt "\nline3\nline4")
  (check (virtual-terminal-screen vt)
    =>
    '("abcld     " "line3     " "line4     ")
  ) ;check
  (check (virtual-terminal-raw vt)
    =>
    '("hello" "\nworld" "\rabc" "\nline3\nline4")
  ) ;check
) ;let

(display "=== 验收点 7: 光标移动、边界安全约束、显隐控制与清屏 / 清行 ===\n"
) ;display
(let ((vt (make-virtual-terminal :width 20 :height 5)))
  (terminal-set-cursor! vt 5 2)
  (check (terminal-cursor vt) => '(5 2))
  (terminal-set-cursor! vt -10 -5)
  (check (terminal-cursor vt) => '(0 0))
  (terminal-set-cursor! vt 999 999)
  (check (terminal-cursor vt) => '(19 4))
  (terminal-hide-cursor! vt)
  (check (terminal-cursor-visible? vt) => #f)
  (terminal-show-cursor! vt)
  (check (terminal-cursor-visible? vt) => #t)
  (terminal-set-cursor! vt 0 1)
  (terminal-write vt "first line")
  (terminal-clear-line vt)
  (check (cadr (virtual-terminal-screen vt)) => (make-string 20 #\space))
  (terminal-clear vt)
  (check (terminal-cursor vt) => '(0 0))
  (check (virtual-terminal-screen vt) => (make-list 5 (make-string 20 #\space)))
) ;let

(display "=== 验收点 8: 动态调整尺寸 (virtual-terminal-resize!) 与行缓冲适配 ===\n"
) ;display
(let ((vt (make-virtual-terminal :width 10 :height 4)))
  (terminal-write vt "0123456789\nabc")
  (terminal-set-cursor! vt 9 3)
  (virtual-terminal-resize! vt 15 2)
  (check (terminal-size vt) => '(15 2))
  (check (terminal-cursor vt) => '(9 1))
  (check (virtual-terminal-screen vt) => '("0123456789     " "abc            "))
  (virtual-terminal-resize! vt 5 3)
  (check (terminal-size vt) => '(5 3))
  (check (terminal-cursor vt) => '(4 1))
  (check (virtual-terminal-screen vt) => '("01234" "abc  " "     "))
) ;let

(display "=== 验收点 9: standard-terminal 真实终端抽象与 ANSI 转义序列 ===\n"
) ;display
(let* ((p (open-output-string))
       (st (make-standard-terminal :output-port p :detect-size-fn (lambda () '(80
                                                                               25)))
       ) ;st
      ) ;
  (check (terminal? st) => #t)
  (check (standard-terminal? st) => #t)
  (check (terminal-size st) => '(80 25))
  (terminal-write st "text")
  (check (get-output-string p) => "text")
  (let ((p2 (open-output-string)))
    (let ((st2 (make-standard-terminal :output-port p2)))
      (terminal-set-cursor! st2 4 2)
      (check (terminal-cursor st2) => '(4 2))
      (check (get-output-string p2) => "\x1b;[3;5H")
      (let ((p3 (open-output-string)))
        (let ((st3 (make-standard-terminal :output-port p3)))
          (terminal-clear st3)
          (check (terminal-cursor st3) => '(0 0))
          (terminal-clear-line st3)
          (terminal-hide-cursor! st3)
          (terminal-show-cursor! st3)
          (check (get-output-string p3)
            =>
            "\x1b;[2J\x1b;[H\x1b;[2K\r\x1b;[?25l\x1b;[?25h"
          ) ;check
          (check (terminal-flush st3) => #t)
        ) ;let
      ) ;let
    ) ;let
  ) ;let
) ;let*

(display "=== 验收点 10: 顶级门面导出与类型拦截异常 ===\n")
(let ((vt (make-virtual-terminal :width 10 :height 5)))
  (check-catch 'type-error (terminal-write vt 123))
  (check-catch 'type-error (terminal-write "not a term" "text"))
  (check-catch 'type-error (terminal-set-cursor! vt "0" 0))
  (check-catch 'type-error (terminal-set-cursor! vt 0 1.5))
  (check-catch 'type-error (virtual-terminal-screen "not a vt"))
  (check-catch 'type-error (virtual-terminal-raw "not a vt"))
  (check-catch 'type-error (virtual-terminal-resize! "not a vt" 10 5))
  (check-catch 'type-error (make-virtual-terminal :width "abc"))
  (check-catch 'value-error (make-virtual-terminal :width 0 :height 10))
  (check-catch 'type-error (make-standard-terminal :output-port "not a port"))
) ;let

(check-report)
