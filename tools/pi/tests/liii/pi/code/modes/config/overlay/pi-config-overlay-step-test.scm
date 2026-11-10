;; pi-config-overlay-step
;; 处理只读覆盖层上的按键，返回新的视图与滚动偏移。
;;
;; 语法
;; ----
;; (pi-config-overlay-step view offset height key)
;;
;; 参数
;; ----
;; view : 覆盖层视图
;; pi-config-file-view 生成的文件视图。
;;
;; offset : integer
;; 当前滚动偏移。
;;
;; height : integer
;; 终端高度。
;;
;; key : (or symbol string)
;; 按键事件。
;;
;; 返回值
;; ----
;; list
;; 包含新视图与新滚动偏移的列表，退出时视图为 #f。

(import (scheme base)
  (liii check)
  (liii list)
  (liii pi code modes config state)
  (liii pi code modes config layout)
  (liii pi code modes config overlay)
  (liii pi code modes config files)
) ;import

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(define DIR (string-append FIXTURE "/config-tui"))

(define st (pi-config-state DIR :env '()))

(define (p-index id)
  (list-index (lambda (p) (string=? (pi-config-provider-id p) id))
    (pi-config-state-providers st)
  ) ;list-index
) ;define

(define view
  (pi-config-file-view st (pi-config-cursor (p-index "kimi-coding") 0) 0)
) ;define

;; 1. 上下滚动
(check (pi-config-overlay-step view 0 20 'down) => (list view 1))
(check (pi-config-overlay-step view 1 20 'up) => (list view 0))

;; 2. 越界滚动不生效
(check (pi-config-overlay-step view 0 20 'up) => (list view 0))
(check (pi-config-overlay-step view 5 20 "g") => (list view 0))

;; 3. vim 风格按键
(check (pi-config-overlay-step view 0 20 "j") => (list view 1))
(check (pi-config-overlay-step view 1 20 "k") => (list view 0))

;; 4. 退出覆盖层
(check (pi-config-overlay-step view 2 20 'escape) => (list #f 0))
(check (pi-config-overlay-step view 2 20 "q") => (list #f 0))

(check-report)
