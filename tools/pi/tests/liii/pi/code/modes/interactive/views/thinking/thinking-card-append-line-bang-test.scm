;; thinking-card-append-line!
;; 向思维链卡片中追加一行新的思考输出文本。
;;
;; 语法
;; ----
;; (thinking-card-append-line! card line)
;;
;; 参数
;; ----
;; card : component?
;; 思维链卡片组件。
;; line : string?
;; 待追加的单行文本。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive views thinking)
) ;import

(check-set-mode! 'report-failed)

;; 1. 追加行后渲染输出包含新增内容
(let* ((card (make-thinking-card "step 1")) (lines1 (component-render card 80)))
  (check-false (string-contains? (string-join lines1 "\n") "step 2"))
  (thinking-card-append-line! card "step 2")
  (let ((lines2 (component-render card 80)))
    (check-true (string-contains? (string-join lines2 "\n") "step 2"))
  ) ;let
) ;let*

(check-report)
