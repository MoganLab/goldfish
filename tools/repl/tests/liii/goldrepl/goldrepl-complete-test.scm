(import (liii check)
  (liii list)
  (liii os)
  (liii string)
  (liii tui)
  (liii goldrepl)
) ;import

;; 补全单候选与多候选逻辑深度测试
;; 单候选：string-app -> string-append
(let* ((ed (make-text-input "string-app"))
       (cands (%repl-completions "string-app"))
       (app-out! (lambda (l) #f))
      ) ;
  (let-values (((rest-cands idx ok?)
                (%repl-try-complete-first! ed "string-app" '(0 10) "string-app" cands)
               ) ;
              ) ;
    (check ok? => #t)
    (check (editor-text ed) => "string-append")
  ) ;let-values
) ;let*

;; 补全无候选时不崩溃测试：%repl-try-complete-first!
(let* ((ed (make-text-input "dd")))
  (let-values (((cands idx ok?) (%repl-try-complete-first! ed "dd" '(0 2) "dd" '())))
    (check cands => '())
    (check idx => 0)
    (check ok? => #f)
  ) ;let-values
) ;let*

;; F1 编辑帮助测试：%repl-editor-help-lines
(let ((lines (%repl-editor-help-lines)))
  (check-true (pair? lines))
  (check-true (any (lambda (l) (string-contains l "Editing Commands")) lines))
  (check-true (any (lambda (l) (string-contains l "shift-tab")) lines))
  (check-true (any (lambda (l) (string-contains l "ctrl-a")) lines))
  (check-true (any (lambda (l) (string-contains l "ctrl-k")) lines))
) ;let

;; 补全菜单排版测试：%repl-format-completion-menu
(let* ((cands '(("string" . "doc1") ("string?" . "doc2") ("string=?" . "doc3")))
       (lines (%repl-format-completion-menu cands))
      ) ;
  (check-true (pair? lines))
  (check-true (string-contains (car lines) "1"))
  (check-true (string-contains (car lines) "string"))
) ;let*

;; 补全多候选生命周期测试：app-out! 不应被调用，scroll 历史不应被污染
(let* ((ed (make-text-input "string->"))
       (cands (%repl-completions "string->"))
       (output-lines '())
       (app-out! (lambda (l) (set! output-lines (append output-lines l))))
      ) ;
  (check-true (pair? cands))
  (let-values (((rest-cands idx ok?)
                (%repl-try-complete-first! ed "string->" '(0 8) "string->" cands)
               ) ;
              ) ;
    (check ok? => #t)
    (check (pair? rest-cands) => #t)
    ;; 验证 app-out! 没有被调用，scroll 历史保持干净
    (check output-lines => '())
  ) ;let-values
) ;let*

;; 瞬态补全菜单展示与选定后消失测试（virtual-terminal）
(let* ((vt (make-virtual-terminal :width 60 :height 10))
       (screen (make-main-screen vt))
       (ed (make-text-input "string->"))
       (scroll '())
       (cands (%repl-completions "string->"))
       (menu (%repl-format-completion-menu cands 56))
      ) ;
  ;; 1. Tab 触发补全时：瞬态菜单作为 extra-lines 渲染在输入行下方
  (%repl-refresh! screen vt scroll ed 60 #f menu)
  (check (> (length (screen-lines screen)) 1) => #t)
  (check (string-contains (cadr (screen-lines screen)) "1") => #t)
  ;; 2. 按下 1 选定候选 1 后：comp-cands 清空，下一帧渲染时 menu 为 '()
  (editor-set-text! ed "string->list")
  (editor-set-cursor! ed 0 12)
  (%repl-refresh! screen vt scroll ed 60 #f '())
  ;; 3. 验证屏幕行仅剩输入行，下方补全选项彻底消失
  (check (length (screen-lines screen)) => 1)
  (check (string-contains (car (screen-lines screen)) "string->list") => #t)
) ;let*

;; 瞬态补全菜单消失与终端清行动作测试（standard-terminal :inline #t）
(let* ((p (open-output-string))
       (st (make-standard-terminal :output-port p :inline #t))
       (screen (make-main-screen st :auto-hide-cursor #f))
       (ed (make-text-input "string->"))
       (scroll '())
       (cands (%repl-completions "string->"))
       (menu (%repl-format-completion-menu cands 56))
      ) ;
  ;; 1. 第一帧：带菜单渲染
  (%repl-refresh! screen st scroll ed 60 #f menu)
  ;; 2. 第二帧：选定候选，重置菜单
  (editor-set-text! ed "string->list")
  (editor-set-cursor! ed 0 12)
  (%repl-refresh! screen st scroll ed 60 #f '())
  ;; 3. 验证屏幕行中菜单行被消除
  (check (length (screen-lines screen)) => 1)
  ;; 4. 验证向终端发送了包含 \x1b;[2K 的清行动作
  (check (string-contains (get-output-string p) "\x1b;[2K") => #t)
) ;let*

;; 替换词时光标定位测试（中间词替换）
(let* ((ed (make-text-input "(string-> 123)"))
       (txt (editor-text ed))
       (cur '(0 9))
       (w (%repl-word-at txt (cadr cur)))
       (cand "string->list")
       (col (cadr cur))
       (start (- col (string-length w)))
      ) ;
  (editor-set-text! ed (%repl-replace-word txt col w cand))
  (editor-set-cursor! ed 0 (+ start (string-length cand)))
  (check (editor-text ed) => "(string->list 123)")
  (check (editor-cursor ed) => '(0 13))
) ;let*

(check-report)
