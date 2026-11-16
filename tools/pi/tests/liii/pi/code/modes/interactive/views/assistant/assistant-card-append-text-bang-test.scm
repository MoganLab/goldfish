;; assistant-card-append-text!
;; 向助手正文卡片追加增量文本并触发缓存失效重渲染。
;;
;; 语法
;; ----
;; (assistant-card-append-text! card delta)
;;
;; 参数
;; ----
;; card : component?
;; 由 make-assistant-card 创建的助手正文卡片。
;;
;; delta : string?
;; 追加的文本增量。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii pi code modes interactive views assistant)
) ;import

(check-set-mode! 'report-failed)

(let* ((card (make-assistant-card "Hello")) (lines1 (component-render card 40)))
  (check (assistant-card-component? card) => #t)
  (check (assistant-card? (component-data card)) => #t)
  (check (assistant-card-text card) => "Hello")
  (check lines1 => '("Hello" ""))

  (assistant-card-append-text! card ", ")
  (assistant-card-append-text! card "world!")
  (check (assistant-card-text card) => "Hello, world!")
  (check (component-render card 40) => '("Hello, world!" ""))

  ;; 空增量或非字符串不改变已有文本
  (assistant-card-append-text! card "")
  (assistant-card-append-text! card #f)
  (check (assistant-card-text card) => "Hello, world!")

  ;; 追加换行与超宽文本可正常折行
  (assistant-card-append-text! card "\nsecond line")
  (check (component-render card 40) => '("Hello, world!" "second line" ""))
) ;let*

(check-report)
