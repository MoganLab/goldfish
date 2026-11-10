;; spinner-success!
;; 将微调器状态置为成功（'success），可选更新文本。
;;
;; 语法
;; ----
;; (spinner-success! obj [text])
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

(let ((s (make-spinner "Build" :frames 'line)))
  (check (spinner-success! s "Build passed") => #t)
  (check (spinner-status s) => 'success)
  (check (spinner-text s) => "Build passed")
  (check (spinner-current-frame s) => "✔")
) ;let

(let ((s (make-spinner)))
  (check-catch 'type-error (spinner-success! s 123))
  (check-catch 'type-error (spinner-success! "not-a-spinner"))
) ;let

(check-report)
