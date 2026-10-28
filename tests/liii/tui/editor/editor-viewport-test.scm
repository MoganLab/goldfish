(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii tui component)
) ;import

(check-set-mode! 'report-failed)

;; 1. 未设置 max-lines：渲染全部行，保持旧行为
(let ((ed (make-editor "a\nb\nc" :multiline #t)))
  (check (length (component-render ed 20)) => 3)
  (check (editor-top-row ed) => 0)
  (check (editor-viewport ed) => '(0 3 0 0))
) ;let

;; 2. 超过上限：窗口贴底，顶部显示计数
(let ((ed (make-editor "l0\nl1\nl2\nl3" :multiline #t :max-lines 2)))
  (editor-set-cursor! ed 3 2)
  (let ((lines (component-render ed 20)))
    (check (length lines) => 3)
    (check (string-contains? (car lines) "↑ 2") => #t)
    (check (list-ref lines 1) => "l2")
    (check (string-contains? (list-ref lines 2) "l3") => #t)
  ) ;let
  (check (editor-top-row ed) => 2)
  (check (editor-viewport ed) => '(2 2 2 0))
) ;let

;; 3. 窗口内移动不滚动；越过上边界才滚动
(let ((ed (make-editor "l0\nl1\nl2\nl3" :multiline #t :max-lines 2)))
  (component-render ed 20)
  (editor-set-cursor! ed 2 0)
  (component-render ed 20)
  (check (editor-top-row ed) => 1)
  (editor-set-cursor! ed 1 0)
  (component-render ed 20)
  (check (editor-top-row ed) => 1)
  (editor-set-cursor! ed 0 0)
  (component-render ed 20)
  (check (editor-top-row ed) => 0)
) ;let

;; 4. 末尾换行自动向下滚动
(let ((ed (make-editor "l0\nl1\nl2" :multiline #t :max-lines 2)))
  (editor-set-cursor! ed 2 2)
  (component-render ed 20)
  (check (editor-top-row ed) => 1)
  (check (component-handle-input ed 'ctrl-j) => #t)
  (component-render ed 20)
  (check (editor-top-row ed) => 2)
  (check (editor-viewport ed) => '(2 2 2 0))
) ;let

;; 5. 上限大于总行数：全部显示且无指示行
(let ((ed (make-editor "a\nb" :multiline #t :max-lines 5)))
  (check (length (component-render ed 20)) => 2)
) ;let

;; 6. 中间窗口：上下都有指示
(let* ((text (string-join (map number->string '(0 1 2 3 4 5 6 7 8 9)) "\n"))
       (ed (make-editor text :multiline #t :max-lines 3))
      ) ;
  (editor-set-cursor! ed 5 0)
  (let ((lines (component-render ed 20)))
    (check (length lines) => 5)
    (check (string-contains? (car lines) "↑ 3") => #t)
    (check (string-contains? (list-ref lines 1) "3") => #t)
    (check (string-contains? (list-ref lines 3) "5") => #t)
    (check (string-contains? (list-ref lines 4) "↓ 4") => #t)
  ) ;let
  (check (editor-viewport ed) => '(3 3 3 4))
) ;let*

;; 7. 合并行后视口重新夹取
(let ((ed (make-editor "l0\nl1\nl2\nl3" :multiline #t :max-lines 2)))
  (editor-set-cursor! ed 3 2)
  (component-render ed 20)
  (editor-set-cursor! ed 2 0)
  (component-handle-input ed 'backspace)
  (component-render ed 20)
  (check (editor-top-row ed) => 1)
) ;let

;; 8. 长逻辑行按宽度折行，视口跟踪其视觉光标所在行
(let ((ed (make-editor "one two three four" :max-lines 2 :cursor-style 'hardware)))
  (let ((lines (component-render ed 7)))
    (check (length lines) => 3)
    (check (string-contains? (list-ref lines 1) "three") => #t)
    (check (list-ref lines 2) => "four")
    (check (editor-viewport ed 7) => '(1 2 1 0))
    (check (editor-display-position ed 7) => '(1 4))
  ) ;let
) ;let

(check-report)
