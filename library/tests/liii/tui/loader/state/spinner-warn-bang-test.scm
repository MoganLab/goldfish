;; spinner-warn!
;; 将微调器状态置为警告（'warning），可选更新文本。
;;
;; 语法
;; ----
;; (spinner-warn! obj [text])
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; text : string? (可选)
;; 更新的提示文本。
;;
;; 返回值
;; ----
;; boolean?
;; 操作成功返回 #t。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((s (make-spinner "Lint")))
  (check (spinner-warn! s "Lint warnings detected") => #t)
  (check (spinner-status s) => 'warning)
  (check (spinner-text s) => "Lint warnings detected")
  (check (spinner-current-frame s) => "⚠")
) ;let

(let ((s (make-spinner)))
  (check-catch 'type-error (spinner-warn! s 123))
  (check-catch 'type-error (spinner-warn! "not-a-spinner"))
) ;let

(check-report)
