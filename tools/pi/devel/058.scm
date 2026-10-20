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
  (liii tui box)
  (liii tui box border)
  (liii tui box box)
  (liii tui box vstack)
  (liii tui box hstack)
  (liii tui box hstack width)
) ;import

(check-set-mode! 'report-failed)

;; [058] 验收脚本：(liii tui box) 与布局容器组件
;; 逐条对应 tools/pi/devel/058.md 验收点：
;;   1) 门禁检查：构建 / fmt / check / doc
;;   2) 单文件行数 <= 300 行硬约束（代码与测试文件）
;;   3) 单函数单元测试与回归测试全量通过（CLI 执行）
;;   4) make-box 边框、内边距、标题与安全边界处理
;;   5) make-vstack 垂直拼接、spacing 间隔与事件级联
;;   6) make-hstack 水平分列、列宽分配、valign 对齐与 CJK/ANSI 视觉宽度对齐
;;   7) 复杂嵌套布局 (box 嵌套 vstack 嵌套 hstack 嵌套 box) 与多级尺寸动态响应
;;   8) (liii tui) 顶级门面与参数异常类型拦截

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-058-" tag ".txt")))
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

(display "=== 验收点 2: 任务 058 相关代码与测试文件行数硬约束 (<= 300 行) ===\n"
) ;display
(let ((files '("goldfish/liii/tui.scm" "goldfish/liii/tui/component.scm"
               "goldfish/liii/tui/box.scm" "goldfish/liii/tui/box/border.scm"
               "goldfish/liii/tui/box/box.scm"
               "goldfish/liii/tui/box/vstack.scm"
               "goldfish/liii/tui/box/hstack.scm"
               "goldfish/liii/tui/box/hstack/width.scm"
               "tests/liii/tui-test.scm"
               "tests/liii/tui/box/border-style-test.scm"
               "tests/liii/tui/box/make-box-test.scm"
               "tests/liii/tui/box/make-vstack-test.scm"
               "tests/liii/tui/box/make-hstack-test.scm"
               "tests/liii/tui/box/nested-layout-test.scm"
               "tools/pi/devel/058.scm")
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

(display "=== 验收点 3: 单函数单元测试全量通过 (CLI 执行) ===\n"
) ;display
(for-each
  (lambda (test-file)
    (check (%run-ok? "unit-test" (string-append "bin/gf " test-file) 30) => 0)
  ) ;lambda
  '("tests/liii/tui/box/border-style-test.scm"
    "tests/liii/tui/box/make-box-test.scm"
    "tests/liii/tui/box/make-vstack-test.scm"
    "tests/liii/tui/box/make-hstack-test.scm"
    "tests/liii/tui/box/nested-layout-test.scm" "tests/liii/tui-test.scm")
) ;for-each

(display "=== 验收点 4: make-box 风格、内边距、标题与安全边界 ===\n"
) ;display
;; 1. 风格渲染
(let ((b-single (make-box "A" :style 'single))
      (b-ascii (make-box "A" :style 'ascii))
      (b-double (make-box "A" :style 'double))
      (b-rounded (make-box "A" :style 'rounded))
      (b-none (make-box "A" :style 'none))
     ) ;
  (check (car (component-render b-single 5)) => "┌───┐")
  (check (car (component-render b-ascii 5)) => "+---+")
  (check (car (component-render b-double 5)) => "╔═══╗")
  (check (car (component-render b-rounded 5)) => "╭───╮")
  (check (car (component-render b-none 5)) => "A    ")
) ;let

;; 2. 内边距与标题
(let* ((b (make-box "hi" :style 'single :padding-left 1 :padding-top 1 :title "T"))
       (lines (component-render b 10))
      ) ;
  (check (display-width (car lines)) => 10)
  (check (car lines) => "┌─ T ────┐")
  (check (list-ref lines 1) => "│        │")
  (check (list-ref lines 2) => "│ hi     │")
  (check (list-ref lines 3) => "└────────┘")
) ;let*

;; 3. 边界收缩不崩溃
(let ((b (make-box "text" :style 'ascii)))
  (check (component-render b 0) => '())
  (check (component-render b 1) => '("+" "|" "+"))
  (check (component-render b 2) => '("++" "||" "++"))
) ;let

(display "=== 验收点 5: make-vstack 垂直堆叠、间隔与事件级联 ===\n"
) ;display
(let* ((v (make-vstack (list "line 1" "line 2") :spacing 1))
       (lines (component-render v 15))
      ) ;
  (check (length lines) => 3)
  (check (list-ref lines 0) => "line 1")
  (check (list-ref lines 1) => "")
  (check (list-ref lines 2) => "line 2")
) ;let*

;; 键盘事件消费短路
(let* ((called '())
       (c1
         (make-component
           :render       (lambda (w) '("1"))
           :handle-input (lambda (k) (set! called (cons 1 called)) #f)
         ) ;make-component
       ) ;c1
       (c2
         (make-component
           :render       (lambda (w) '("2"))
           :handle-input (lambda (k) (set! called (cons 2 called)) #t)
         ) ;make-component
       ) ;c2
       (c3
         (make-component
           :render       (lambda (w) '("3"))
           :handle-input (lambda (k) (set! called (cons 3 called)) #t)
         ) ;make-component
       ) ;c3
       (v (make-vstack (list c1 c2 c3)))
      ) ;
  (check (component-handle-input v 'up) => #t)
  (check (reverse called) => '(1 2))
) ;let*

(display "=== 验收点 6: make-hstack 水平分列与 CJK/ANSI 视觉宽度对齐 ===\n"
) ;display
;; 两列均分
(let* ((h (make-hstack "colA" "colB" :spacing 2)) (lines (component-render h 20)))
  (check (length lines) => 1)
  (check (display-width (car lines)) => 20)
) ;let*

;; 中文与 ANSI 混排严格对齐
(let* ((h (make-hstack (list "中文标题" "\x1b;[33mWarning\x1b;[0m") :widths '(12
                                                                                  12))
       ) ;h
       (lines (component-render h 24))
      ) ;
  (check (length lines) => 1)
  (check (display-width (car lines)) => 24)
) ;let*

;; 垂直对齐 valign: 'bottom
(let* ((h (make-hstack (list (make-text "1\n2") (make-text "3"))
            :valign 'bottom
            :widths '(5 5)
          ) ;make-hstack
       ) ;h
       (lines (component-render h 10))
      ) ;
  (check (length lines) => 2)
  (check (list-ref lines 0) => "1         ")
  (check (list-ref lines 1) => "2    3    ")
) ;let*

(display "=== 验收点 7: 复杂嵌套布局与多级尺寸动态响应 ===\n")
(let* ((left-pane (make-box "Nav List" :style 'single :title "Nav"))
       (right-pane (make-box "Main Body" :style 'single :title "Content"))
       (body (make-hstack (list left-pane right-pane) :spacing 2))
       (app (make-box (make-vstack (list "Header" body "Footer") :spacing 1)
              :style 'rounded
              :title "Dashboard"
            ) ;make-box
       ) ;app
      ) ;
  ;; 在 60 宽渲染
  (let ((lines60 (component-render app 60)))
    (check (> (length lines60) 5) => #t)
    (for-each (lambda (l) (check (display-width l) => 60)) lines60)
  ) ;let
  ;; 在 40 宽收缩自适应渲染
  (let ((lines40 (component-render app 40)))
    (check (> (length lines40) 5) => #t)
    (for-each (lambda (l) (check (display-width l) => 40)) lines40)
  ) ;let
) ;let*

(display "=== 验收点 8: 门面导出与异常拦截 ===\n")
(check (box? (make-box "hi")) => #t)
(check (vstack? (make-vstack "a" "b")) => #t)
(check (hstack? (make-hstack "a" "b")) => #t)
(check (border-style? (make-border-style "+" "+" "+" "+" "-" "|")) => #t)
(check-catch 'type-error (make-box 100))
(check-catch 'value-error (make-vstack "a" :spacing -1))
(check-catch 'value-error (make-hstack "a" "b" :widths '(10)))

(check-report)
