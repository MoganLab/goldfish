(import (liii check) (liii tui component) (liii tui editor component))

(check-set-mode! 'report-failed)

;; 多行编辑器的上下键语义（对齐上游 packages/tui/src/components/editor.ts）：
;;   ↑：光标在首行时回溯输入历史（编辑器为空 / 正在浏览历史 / 光标在行首时
;;       直接回溯，否则先跳到行首）；不在首行时移动光标。
;;   ↓：浏览历史中且位于末行时回退历史；位于末行时跳到行尾；否则移动光标。
;;
;; 背景：pi 的输入框以 :multiline #t 构造，此前 ↑ 永远落在 buffer-move-up!
;; 分支（单行内容时为空操作），导致 ESC 中断后无法用 ↑ 回溯上一条 prompt。

;; 1. 多行编辑器：空输入时按 ↑ 直接回溯历史（pi 中断后的典型场景）
(let* ((ed (make-editor "" :multiline #t)) (submitted '()))
  (component-handle-input ed "first")
  (component-handle-input ed 'enter)
  (editor-set-text! ed "")
  (check (editor-text ed) => "")
  (check (component-handle-input ed 'up) => #t)
  (check (editor-text ed) => "first")
) ;let*

;; 2. 多行编辑器：有草稿且光标不在行首时，↑ 先跳到行首，再按一次回溯历史
(let ((ed (make-editor "" :multiline #t)))
  (component-handle-input ed "old")
  (component-handle-input ed 'enter)
  (editor-set-text! ed "")
  (component-handle-input ed "draft")
  (check (editor-cursor ed) => '(0 5))
  ;; 第一次 ↑：跳到行首（不回溯历史，草稿保持不变）
  (check (component-handle-input ed 'up) => #t)
  (check (editor-cursor ed) => '(0 0))
  (check (editor-text ed) => "draft")
  ;; 第二次 ↑：光标在行首 → 回溯历史
  (check (component-handle-input ed 'up) => #t)
  (check (editor-text ed) => "old")
) ;let

;; 3. 多行编辑器：↑ 回溯后光标落在行首，连续 ↑ 可继续回溯更早条目
(let ((ed (make-editor "" :multiline #t)))
  (component-handle-input ed "one")
  (component-handle-input ed 'enter)
  (editor-set-text! ed "")
  (component-handle-input ed "two")
  (component-handle-input ed 'enter)
  (editor-set-text! ed "")
  (check (component-handle-input ed 'up) => #t)
  (check (editor-text ed) => "two")
  ;; 回溯后光标置于行尾（与既有行为一致）；
  ;; 浏览历史态下 ↑ 直接继续回溯更早条目，不依赖光标列位置
  (check (editor-cursor ed) => '(0 3))
  (check (component-handle-input ed 'up) => #t)
  (check (editor-text ed) => "one")
  ;; 到底了：仍停在最旧条目
  (check (component-handle-input ed 'up) => #t)
  (check (editor-text ed) => "one")
) ;let

;; 4. 多行编辑器：↓ 在浏览历史且位于末行时回退历史，恢复草稿
(let ((ed (make-editor "" :multiline #t)))
  (component-handle-input ed "one")
  (component-handle-input ed 'enter)
  (editor-set-text! ed "")
  (component-handle-input ed "draft")
  ;; 第一次 ↑ 跳到行首，第二次 ↑ 回溯历史
  (check (component-handle-input ed 'up) => #t)
  (check (component-handle-input ed 'up) => #t)
  (check (editor-text ed) => "one")
  (check (component-handle-input ed 'down) => #t)
  (check (editor-text ed) => "draft")
) ;let

;; 5. 多行编辑器：光标不在首行时 ↑ 只移动光标，不触发历史回溯
(let ((ed (make-editor "" :multiline #t)))
  (component-handle-input ed "one")
  (component-handle-input ed 'enter)
  (editor-set-text! ed "")
  ;; 构造两行内容：ctrl-j 换行
  (component-handle-input ed "line1")
  (component-handle-input ed 'ctrl-j)
  (component-handle-input ed "line2")
  (check (editor-cursor ed) => '(1 5))
  (check (component-handle-input ed 'up) => #t)
  (check (editor-cursor ed) => '(0 5))
  (check (editor-text ed) => "line1\nline2")
) ;let

;; 6. 多行编辑器：位于末行且未浏览历史时 ↓ 跳到行尾
(let ((ed (make-editor "" :multiline #t)))
  (component-handle-input ed "abc")
  (component-handle-input ed 'ctrl-a)
  (check (editor-cursor ed) => '(0 0))
  (check (component-handle-input ed 'down) => #t)
  (check (editor-cursor ed) => '(0 3))
) ;let

;; 7. 单行编辑器保持经典 readline 行为：↑ 始终回溯历史（不跳行首）
(let ((ed (make-editor "")))
  (component-handle-input ed "one")
  (component-handle-input ed 'enter)
  (editor-set-text! ed "")
  (component-handle-input ed "draft")
  (check (component-handle-input ed 'up) => #t)
  (check (editor-text ed) => "one")
) ;let

(check-report)
