;; %repl-render-input-lines
;; 把编辑器内容渲染成带提示符与语法高亮的行列表。
;;
;; 语法
;; ----
;; (%repl-render-input-lines ed width pending?)
;;
;; 参数
;; ----
;; ed : editor
;; 编辑器对象。
;; width : integer
;; 终端宽度。
;; pending? : boolean
;; 为真时表示正在续写多行输入，提示符使用续行提示符。
;;
;; 返回值
;; ----
;; list
;; 渲染之后的文本行列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii goldrepl render)
) ;import

(check-set-mode! 'report-failed)

;; 单行输入
(let* ((ed (make-text-input "abc")) (lines (%repl-render-input-lines ed 20 #f)))
  (check (length lines) => 1)
  ;; 行首是首行提示符
  (check-true (string-contains (car lines) "gf>"))
  ;; 行内包含输入内容
  (check-true (string-contains (car lines) "abc"))
) ;let*

;; 续行输入使用续行提示符
(let* ((ed (make-text-input "abc")) (lines (%repl-render-input-lines ed 20 #t)))
  (check (length lines) => 1)
  (check-true (string-contains (car lines) "..."))
  (check (not (string-contains (car lines) "gf>")) => #t)
) ;let*

;; 空输入只有提示符
(let* ((ed (make-text-input "")) (lines (%repl-render-input-lines ed 20 #f)))
  (check (length lines) => 1)
  (check-true (string-contains (car lines) "gf>"))
) ;let*

(check-report)
