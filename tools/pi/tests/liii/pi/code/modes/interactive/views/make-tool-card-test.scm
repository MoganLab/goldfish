(import (scheme base)
        (liii time)
        (liii check)
        (liii list)
        (liii string)
        (liii tui)
        (liii pi code modes interactive views))

(check-set-mode! 'report-failed)

;; bash 成功卡片：padding、标题、输出与背景
(let* ((tool-card (make-tool-card "bash" "npm test" 'success "Tests passed"))
       (w 50)
       (lines (component-render tool-card w)))
  (check (component? tool-card) => #t)
  (check (length lines) => 6)
  (check (string-contains? (car lines) "48;2;37;65;49") => #t)
  (check-false (string-contains? (car lines) "npm test"))
  (check (string-contains? (cadr lines) "48;2;37;65;49") => #t)
  (check (string-contains? (cadr lines) "$ npm test") => #t)
  (check (string-contains? (cadr lines) "\x1b;[1m\x1b;[38;2;222;224;225m") => #t)
  (check-false (string-contains? (cadr lines) "0;1;38;2"))
  (check (string-contains? (list-ref lines 3) "38;2;157;165;169") => #t)
  (check-false (string-contains? (cadr lines) "38;2;157;165;169"))
  (check (string-contains? (list-ref lines 4) "48;2;37;65;49") => #t)
  (check (list-ref lines 5) => ""))

;; 非 bash read pending：工具名、accent 路径、pending 背景且无徽标
(let* ((pending-card (make-tool-card "read" "file.scm" 'pending))
       (w 50)
       (lines (component-render pending-card 50))
       (plain (string-join (map strip-ansi lines) "\n")))
  (check (string-contains? (cadr lines) "48;2;52;56;58") => #t)
  (check (string-contains? (cadr lines) "\x1b;[1m\x1b;[38;2;222;224;225mread") => #t)
  (check (string-contains? (cadr lines) "38;2;167;152;215m file.scm") => #t)
  (check-false (string-contains? plain "●"))
  ;; 对齐上游 theme.fg 的选择性重置（39/22）：分段间绝不清背景。
  ;; 粗体段以 \x1b[22m\x1b[39m 收尾，其余段以 \x1b[39m 收尾；
  ;; 标题行内不允许出现 \x1b[0m 全重置（末尾 apply-card-bg 的收尾除外）
  (let ((title (cadr lines)))
    (check (string-contains? title "read\x1b;[22m\x1b;[39m") => #t)
    (check (string-contains? title "file.scm\x1b;[39m") => #t)
    (check (= (length (string-split title "\x1b;[0m")) 2) => #t))
  ;; 带 warning 范围后缀的 read 调用：范围段同样只重置前景
  (let* ((range-card (make-tool-card "read"
                         '(("path" . "a.scm") ("offset" . 10) ("limit" . 5))
                         'pending))
         (title (cadr (component-render range-card 60))))
    (check (string-contains? title ":10-14") => #t)
    (check (string-contains? title ":10-14\x1b;[39m") => #t)
    (check (= (length (string-split title "\x1b;[0m")) 2) => #t)))

;; write 失败卡片：红背景、输出 muted，无状态红字和徽标
(let* ((error-card (make-tool-card "write" "out.txt" 'error "Permission denied"))
       (w 50)
       (lines (component-render error-card w))
       (joined (string-join lines "")))
  (check (string-contains? (cadr lines) "48;2;91;40;42") => #t)
  (check (string-contains? (cadr lines) "\x1b;[1m\x1b;[38;2;222;224;225mwrite") => #t)
  (check (string-contains? (cadr lines) "38;2;167;152;215m out.txt") => #t)
  ;; 与 read 相同：分段间只选择性重置，背景铺满整行（write/edit/grep 等共用该路径）
  (check (string-contains? (cadr lines) "write\x1b;[22m\x1b;[39m") => #t)
  (check (string-contains? (cadr lines) "out.txt\x1b;[39m") => #t)
  (check (= (length (string-split (cadr lines) "\x1b;[0m")) 2) => #t)
  (check-false (string-contains? joined "38;2;250;210;210"))
  (check-false (string-contains? joined "✖"))
  (check (string-contains? (list-ref lines 3) "38;2;157;165;169") => #t)
  (check (string-contains? (list-ref lines 3) "Permission denied") => #t))

;; read 成功卡片：绿背景、无成功徽标或绿字，输出背景不中断
(let* ((success-card (make-tool-card "read" "a.scm" 'success "(content)"))
       (w 50)
       (lines (component-render success-card w))
       (joined (string-join lines "")))
  (check (string-contains? (cadr lines) "48;2;37;65;49") => #t)
  (check (string-contains? (cadr lines) "\x1b;[1m\x1b;[38;2;222;224;225mread") => #t)
  (check-false (string-contains? joined "38;2;220;245;220"))
  (check-false (string-contains? joined "✔"))
  (check (string-contains? (list-ref lines 3) "38;2;157;165;169") => #t)
  (check-false (string-contains? (list-ref lines 3) "\x1b;[0m ")))

;; bash 输出内空行的背景铺满整行宽度
(let* ((blank-card (make-tool-card "bash" "printf a" 'success "a\n\nb"))
       (w 50)
       (lines (component-render blank-card w)))
  (check (length lines) => 8)
  (check (string-contains? (list-ref lines 4) "48;2;37;65;49") => #t)
  (check-false (string-contains? (list-ref lines 4) "\x1b;[0m ")))

;; 多行 bash 成功卡片：上下 padding 与输出都保留背景色
(let* ((card (make-tool-card "bash" '(("command" . "git status -s")) 'success " M a.scm\n?? b.scm"))
       (w 40)
       (rendered (component-render card w)))
  (check (component? card) => #t)
  (check (length rendered) => 7)
  (check (string-contains? (car rendered) "48;2;37;65;49") => #t)
  (check-false (string-contains? (car rendered) "git status"))
  (check (string-contains? (cadr rendered) "48;2;37;65;49") => #t)
  (check (string-contains? (cadr rendered) "$ git status -s") => #t)
  (check (string-contains? (list-ref rendered 2) "48;2;37;65;49") => #t)
  (check (string-contains? (list-ref rendered 3) "M a.scm") => #t)
  (check (string-contains? (list-ref rendered 4) "?? b.scm") => #t)
  (check (string-contains? (list-ref rendered 5) "48;2;37;65;49") => #t)
  (check (list-ref rendered 6) => ""))

;; read 失败卡片：红背景同时包住命令行与输出行
(let* ((card (make-tool-card "read" '(("path" . "missing.txt")) 'error "File not found"))
       (rendered (component-render card 40)))
  (check (string-contains? (cadr rendered) "48;2;91;40;42") => #t)
  (check (string-contains? (strip-ansi (cadr rendered)) "read missing.txt") => #t)
  (check (string-contains? (list-ref rendered 3) "File not found") => #t)
  (check (string-contains? (list-ref rendered 3) "48;2;91;40;42") => #t))

;; 运行中显示 Elapsed；结束后使用执行层耗时且不再滚动
(let* ((card (make-tool-card "bash" '(("command" . "sleep 5")) 'pending))
       (pending-text (string-join (map strip-ansi (component-render card 80)) "\n")))
  (check-true (string-contains? pending-text "Elapsed "))
  (tool-card-set-complete! card "done" #f 4200)
  (let ((done-text (string-join (map strip-ansi (component-render card 80)) "\n")))
    (check-true (string-contains? done-text "done"))
    (check-true (string-contains? done-text "Took 4.2s"))
    (check-false (string-contains? done-text "Elapsed"))
    (sleep 0.02)
    (check (string-join (map strip-ansi (component-render card 80)) "\n") => done-text)))

;; 展开状态可往返切换，运行中的日志遵循同一折叠状态
(let ((card (make-tool-card "bash" '(("command" . "seq 1 8")) 'pending "1\n2\n3\n4\n5\n6\n7\n8")))
  (check-false (tool-card-expanded? card))
  (tool-card-set-expanded! card #t)
  (check-true (tool-card-expanded? card))
  (let ((lines (map string-trim-both (map strip-ansi (component-render card 80)))))
    (check-true (pair? (member "1" lines)))
    (check-true (pair? (member "8" lines))))
  (tool-card-set-expanded! card #f)
  (let ((text (string-join (map strip-ansi (component-render card 80)) "\n")))
    (check-true (string-contains? text "... (3 earlier lines, ctrl+o to expand)"))))

;; 非bash工具卡片：Ctrl+O 展开后渲染应显示全部行（复现无法展开的bug）
(let* ((card (make-tool-card "read" "a.scm" 'success "l1\nl2\nl3\nl4\nl5\nl6\nl7\nl8")))
  ;; 折叠态：只显示前5行 + 展开提示
  (let ((text (string-join (map strip-ansi (component-render card 80)) "\n")))
    (check-true (string-contains? text "l1"))
    (check-true (string-contains? text "... (3 more lines, ctrl+o to expand)")))
  ;; 展开态：应显示全部 8 行
  (tool-card-set-expanded! card #t)
  (let ((text (string-join (map strip-ansi (component-render card 80)) "\n")))
    (check-true (string-contains? text "l8"))))

(check-report)
