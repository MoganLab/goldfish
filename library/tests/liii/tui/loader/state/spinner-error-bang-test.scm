;; spinner-error!
;; 将微调器状态置为错误（'error），可选更新文本。
;;
;; 语法
;; ----
;; (spinner-error! obj [text])
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

(let ((s (make-spinner "Compile")))
  (check (spinner-error! s "Syntax error") => #t)
  (check (spinner-status s) => 'error)
  (check (spinner-text s) => "Syntax error")
  (check (spinner-current-frame s) => "✖")
) ;let

(let ((s (make-spinner)))
  (check-catch 'type-error (spinner-error! s 123))
  (check-catch 'type-error (spinner-error! "not-a-spinner"))
) ;let

(check-report)
