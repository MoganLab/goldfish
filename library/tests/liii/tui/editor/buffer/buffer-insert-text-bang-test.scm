;; buffer-insert-text!
;; 在光标当前位置插入可能包含换行符的文本块（支持粘贴展开与多行分裂）。
;;
;; 语法
;; ----
;; (buffer-insert-text! b text)
;;
;; 参数
;; ----
;; b : buffer?
;; 缓冲区实例。
;;
;; text : string?
;; 待插入的文本。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

(let ((b (make-buffer "" #t)))
  (buffer-insert-text! b "line 1\nline 2")
  (check (buffer-lines b) => '("line 1" "line 2"))
  (check (buffer-row b) => 1)
  (check (buffer-col b) => 6)
) ;let

(check-report)
