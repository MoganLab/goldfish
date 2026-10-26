(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
) ;import

(check-set-mode! 'report-failed)

(define (item value description)
  (list (cons 'value value) (cons 'label (string-append "  " value)) (cons 'description description))
) ;define

(define items (list (item "off" "No reasoning") (item "minimal" "Very brief") (item "low" "Light")))
(define selected '())
(define cancel-count 0)
(define lst
  (make-select-list items 2
    :on-select (lambda (entry) (set! selected (cons (cdr (assoc 'value entry)) selected)))
    :on-cancel (lambda () (set! cancel-count (+ cancel-count 1)))
  ) ;make-select-list
) ;define

(check-true (select-list? lst))
(check (select-list-selected-index lst) => 0)
(component-handle-input lst 'down)
(check (select-list-selected-index lst) => 1)
(component-handle-input lst 'down)
(check (select-list-selected-index lst) => 2)
(component-handle-input lst 'down)
(check (select-list-selected-index lst) => 0)
(component-handle-input lst 'up)
(check (select-list-selected-index lst) => 2)
(component-handle-input lst 'enter)
(check selected => '("low"))
(component-handle-input lst 'escape)
(component-handle-input lst 'ctrl-c)
(check cancel-count => 2)

;; 可视窗口、描述列和滚动信息。
(select-list-set-selected-index! lst 0)
(let ((lines (map strip-ansi (component-render lst 80))))
  (check (length lines) => 3)
  (check-true (string-contains? (car lines) "→   off"))
  (check-true (string-contains? (car lines) "No reasoning"))
  (check-true (string-contains? (cadr lines) "minimal"))
  (check (caddr lines) => "  (1/3)")
) ;let

;; 空结果显示上游同款提示，确认键不会误触发回调。
(let ((empty (make-select-list '() 3 :on-select (lambda (entry) (set! selected (cons entry selected))))))
  (check (map strip-ansi (component-render empty 40)) => '("  No matching commands"))
  (component-handle-input empty 'enter)
  (check selected => '("low"))
) ;let

(check-report)
