;; pi-config-aio-modal-render
;; 居中渲染 all-in-one 交互覆盖层。
;;
;; 语法
;; ----
;; (pi-config-aio-modal-render modal width height)
;;
;; 参数
;; ----
;; modal : list
;; 覆盖层状态。
;; width : integer
;; 终端宽度。
;; height : integer
;; 终端高度。
;;
;; 返回值
;; ------
;; list of string
;; 恰好 height 行的渲染帧。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii pi code modes config aio-modal)
  (liii pi code modes config aio-modal-render)
) ;import

(check-set-mode! 'report-failed)

(let* ((m (pi-config-aio-modal-new-chain))
       (lines (pi-config-aio-modal-render m 100 20))
       (txt (strip-ansi (string-join lines "\n")))
      ) ;
  (check (length lines) => 20)
  (check (string-contains? txt "new all-in-one chain") => #t)
  (check (string-contains? txt "id: _") => #t)
) ;let*

(check-report)
