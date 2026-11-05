;; %repl-buffer-set!
;; 替换缓冲区中的多行输入内容。
;;
;; 语法
;; ----
;; (%repl-buffer-set! buf lines)
;;
;; 参数
;; ----
;; buf : list
;; 多行输入缓冲区。
;; lines : list
;; 新的多行输入文本列表。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未定义，函数通过修改缓冲区生效。

(import (scheme base) (liii check) (liii list) (liii goldrepl))

(check-set-mode! 'report-failed)

;; 设置多行输入内容
(let ((buf (%repl-buffer)))
  (%repl-buffer-set! buf '("(define (f x)" "  (+ x 1))"))
  (check (car buf) => '("(define (f x)" "  (+ x 1))"))
  ;; 缓冲区长度不变
  (check (length buf) => 1)
) ;let

;; 可以设置为空行
(let ((buf (%repl-buffer)))
  (%repl-buffer-set! buf "")
  (check (car buf) => "")
) ;let

(check-report)
