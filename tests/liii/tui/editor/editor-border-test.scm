(import (scheme base)
  (liii check)
  (liii tui editor)
  (liii tui component)
) ;import

(check-set-mode! 'report)

;; 1. 默认无边框
(let* ((ed (make-editor "hello" :focused #f))
       (lines (component-render ed 20))
      ) ;let*
  (check (length lines) => 1)
  (check (car lines) => "hello")
) ;let*

;; 2. 开启 top-border 与 bottom-border
(let* ((ed (make-editor "hello" :focused #f :top-border #t :bottom-border #t))
       (lines (component-render ed 10))
      ) ;let*
  (check (length lines) => 3)
  (check (list-ref lines 0) => "──────────")
  (check (list-ref lines 1) => "hello")
  (check (list-ref lines 2) => "──────────")
) ;let*

;; 3. 仅开启 top-border
(let* ((ed (make-editor "hi" :focused #f :top-border #t))
       (lines (component-render ed 6))
      ) ;let*
  (check (length lines) => 2)
  (check (list-ref lines 0) => "──────")
  (check (list-ref lines 1) => "hi")
) ;let*

;; 4. 仅开启 bottom-border
(let* ((ed (make-editor "hi" :focused #f :bottom-border #t))
       (lines (component-render ed 6))
      ) ;let*
  (check (length lines) => 2)
  (check (list-ref lines 0) => "hi")
  (check (list-ref lines 1) => "──────")
) ;let*

(check-report)
