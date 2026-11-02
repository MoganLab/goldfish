(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii pi code modes config state)
  (liii pi code modes config layout)
  (liii pi code modes config overlay)
  (liii pi code modes config files)
) ;import

(check-set-mode! 'report-failed)

;; `v` 覆盖层：只读查看光标条目对应的配置文件
;;   pi-config-file-view     视图（路径/存在性/原文）
;;   pi-config-render-overlay 居中覆盖层（恰好 height 行、每行不超 width）

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(define DIR (string-append FIXTURE "/config-tui"))

(define st (pi-config-state DIR :env '()))

(define (plain s)
  (strip-ansi s)
) ;define

(define (p-index id)
  (list-index (lambda (p) (string=? (pi-config-provider-id p) id))
    (pi-config-state-providers st)
  ) ;list-index
) ;define

;; 1. 有文件：显示路径 + 原文，且标明 read-only
(let* ((view (pi-config-file-view st (pi-config-cursor (p-index "kimi-coding") 0) 0))
       (lines (pi-config-render-overlay view 120 20))
       (text (plain (string-join lines "\n")))
      ) ;
  (check (length lines) => 20)
  (check
    (every
      (lambda (l) (<= (display-width (strip-ansi l)) 120))
      lines
    ) ;every
    =>
    #t
  ) ;check
  (check (string-contains text "models/kimi-coding.json") => #t)
  (check (string-contains text "read-only") => #t)
  (check (string-contains text "Kimi Coding") => #t)
) ;let*

;; 2. 虚拟条目（无文件）：提示 not found + 来源说明
(let* ((view (pi-config-file-view st (pi-config-cursor (p-index "google") 0) 0))
       (text (plain (string-join (pi-config-render-overlay view 120 20) "\n")))
      ) ;
  (check (string-contains text "models/google.json not found") => #t)
  (check (string-contains text "builtin registry") => #t)
) ;let*

;; 3. 模型条目：指向模型文件
(let* ((view (pi-config-file-view st (pi-config-cursor (p-index "kimi-coding") 0) 1))
       (text (plain (string-join (pi-config-render-overlay view 120 20) "\n")))
      ) ;
  (check (string-contains text "models/kimi-coding/k3.1-pro.json") => #t)
  (check (string-contains text "read-only") => #t)
) ;let*

;; 3b. 滚动越界不抛错（take 在行数不足时会报错）
(let ((view (pi-config-file-view st (pi-config-cursor (p-index "kimi-coding") 0) 0)))
  (check (length (pi-config-render-overlay view 120 20 0)) => 20)
  (check (length (pi-config-render-overlay view 120 20 999)) => 20)
) ;let

;; 4. 窄终端：行数固定、宽度受限
(let ((lines
        (pi-config-render-overlay (pi-config-file-view st (pi-config-cursor (p-index "kimi-coding") 0) 0)
          60
          16
        ) ;pi-config-render-overlay
      ) ;lines
     ) ;
  (check (length lines) => 16)
  (check
    (every
      (lambda (l) (<= (display-width (strip-ansi l)) 60))
      lines
    ) ;every
    =>
    #t
  ) ;check
) ;let

;; 4.1 极端高度行数守恒
(let ((view (pi-config-file-view st (pi-config-cursor (p-index "kimi-coding") 0) 0)))
  (check (length (pi-config-render-overlay view 80 3)) => 3)
  (check (length (pi-config-render-overlay view 80 1)) => 1)
  (check (length (pi-config-render-overlay view 80 0)) => 0)
) ;let

;; 5. 覆盖层按键交互（滚动与退出）
(let ((view (pi-config-file-view st (pi-config-cursor (p-index "kimi-coding") 0) 0)))
  (check (pi-config-overlay-step view 0 20 'down) => (list view 1))
  (check (pi-config-overlay-step view 1 20 'up) => (list view 0))
  (check (pi-config-overlay-step view 0 20 'up) => (list view 0))
  (check (pi-config-overlay-step view 0 20 "j") => (list view 1))
  (check (pi-config-overlay-step view 1 20 "k") => (list view 0))
  (check (pi-config-overlay-step view 5 20 "g") => (list view 0))
  (check (cadr (pi-config-overlay-step view 0 20 "G")) => (cadr (pi-config-overlay-step view 0 20 'G)))
  (check (pi-config-overlay-step view 2 20 'escape) => (list #f 0))
  (check (pi-config-overlay-step view 2 20 "q") => (list #f 0))
) ;let

(check-report)
