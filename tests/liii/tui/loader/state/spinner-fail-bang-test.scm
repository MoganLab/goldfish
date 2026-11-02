;; spinner-fail!
;; 将微调器状态置为失败（'error），可选更新文本。
;;
;; 语法
;; ----
;; (spinner-fail! obj [text])
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

(let ((s (make-spinner "Test" :frames 'line)))
  (check (spinner-fail! s "Test failed: timeout") => #t)
  (check (spinner-status s) => 'error)
  (check (spinner-text s) => "Test failed: timeout")
  (check (spinner-current-frame s) => "✖")
) ;let

(let ((s (make-spinner)))
  (check-catch 'type-error (spinner-fail! s 123))
  (check-catch 'type-error (spinner-fail! "not-a-spinner"))
) ;let

(check-report)
