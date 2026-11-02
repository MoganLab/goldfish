(import (liii check)
  (liii tui editor)
) ;import

(check-set-mode! 'report-failed)

;; editor-display-col 终端物理显示列宽计算测试

;; 1. 空编辑器初始为 0
(let ((ed (make-editor "")))
  (check (editor-display-col ed) => 0)
) ;let

;; 2. 纯 ASCII 文本
(let ((ed (make-editor "hello world")))
  ;; 行首
  (editor-set-cursor! ed 0 0)
  (check (editor-display-col ed) => 0)
  ;; 中间
  (editor-set-cursor! ed 0 5)
  (check (editor-display-col ed) => 5)
  ;; 行尾
  (editor-set-cursor! ed 0 11)
  (check (editor-display-col ed) => 11)
) ;let

;; 3. 全角中文字符 (每个汉字占 2 列物理宽度)
(let ((ed (make-editor "你好世界")))
  ;; col=0 -> 物理列 0
  (editor-set-cursor! ed 0 0)
  (check (editor-display-col ed) => 0)
  ;; col=1 ("你" 后面) -> 物理列 2
  (editor-set-cursor! ed 0 1)
  (check (editor-display-col ed) => 2)
  ;; col=2 ("你好" 后面) -> 物理列 4
  (editor-set-cursor! ed 0 2)
  (check (editor-display-col ed) => 4)
  ;; col=4 (行尾) -> 物理列 8
  (editor-set-cursor! ed 0 4)
  (check (editor-display-col ed) => 8)
) ;let

;; 4. 中英混合文本
(let ((ed (make-editor "Hi金鱼")))
  ;; "Hi" 后面 (col=2) -> 物理列 2
  (editor-set-cursor! ed 0 2)
  (check (editor-display-col ed) => 2)
  ;; "Hi金" 后面 (col=3) -> 物理列 4
  (editor-set-cursor! ed 0 3)
  (check (editor-display-col ed) => 4)
  ;; 行尾 (col=4) -> 物理列 6
  (editor-set-cursor! ed 0 4)
  (check (editor-display-col ed) => 6)
) ;let

;; 5. 类型错误校验
(check-catch 'type-error (editor-display-col "not-an-editor"))
(check-catch 'type-error (editor-display-col 123))

(check-report)
