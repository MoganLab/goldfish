;; pi-config-aio-render-body
;; 渲染 all-in-one 子标签页的 ranger 风格三列主体。
;;
;; 语法
;; ----
;; (pi-config-aio-render-body aio cursor active w1 w2 w3 content-rows box-fn list-fn entry-fn detail-fn)
;;
;; 参数
;; ----
;; aio : list
;; all-in-one 状态。
;; cursor : list
;; 光标位置。
;; active : integer
;; 激活列。
;; w1, w2, w3 : integer
;; 三列宽度。
;; content-rows : integer
;; 内容行数。
;; box-fn, list-fn, entry-fn, detail-fn : procedure
;; 渲染辅助回调。
;;
;; 返回值
;; ------
;; list of string
;; 渲染行列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii pi code modes config state)
  (liii pi code modes config layout)
  (liii pi code modes config all-in-one)
  (liii pi code modes config layout-aio)
) ;import

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(define st (pi-config-state (string-append FIXTURE "/config-tui") :env '()))

(define aio (pi-config-aio-state st))

(define (stub-box title lines width active? rows)
  (cons title lines)
) ;define

(define (stub-list items count selected render)
  (map (lambda (i) (render (list-ref items i) (= i selected)))
    (iota (length items))
  ) ;map
) ;define

(define (stub-entry text enabled? default? selected?)
  (string-append (if enabled? "● " "× ") text (if default? " *" ""))
) ;define

(define (stub-detail rows)
  (map (lambda (r) (string-append (car r) ": " (cadr r))) rows)
) ;define

(let ((out (pi-config-aio-render-body aio
             '((chain . 0) (node . 0))
             0
             22
             34
             64
             5
             stub-box
             stub-list
             stub-entry
             stub-detail
           ) ;pi-config-aio-render-body
      ) ;out
     ) ;
  (check (pair? out) => #t)
  (check (string-contains? (car out) "chains · 2") => #t)
  (check (string-contains? (car out) "nodes · coding-fast") => #t)
  (check (string-contains? (car out) "preview · coding-fast") => #t)
) ;let

(check-report)
