;; pi-config-file-view
;; 返回光标所在 provider 或模型对应的配置文件路径与原文。
;;
;; 语法
;; ----
;; (pi-config-file-view st cursor . opt-column)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置 TUI 的只读视图模型。
;; cursor : alist
;; provider 与模型光标位置。
;; opt-column : integer
;; 0 表示 provider 列，1 表示模型列。
;;
;; 返回值
;; ----
;; alist
;; 包含 kind、id、path、exists 与 content 的只读视图。

(import (liii check)
  (liii list)
  (liii string)
  (liii pi code modes config state)
  (liii pi code modes config layout)
  (liii pi code modes config files)
) ;import

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(define (ref alist key)
  (let ((pair (assoc key alist)))
    (and pair (cdr pair))
  ) ;let
) ;define

(define (p-index st id)
  (list-index (lambda (p) (string=? (pi-config-provider-id p) id))
    (pi-config-state-providers st)
  ) ;list-index
) ;define

(let* ((st (pi-config-state (string-append FIXTURE "/config-tui") :env '()))
       (v (pi-config-file-view st (pi-config-cursor (p-index st "kimi-coding") 0) 0))
      ) ;
  (check (ref v "kind") => "provider")
  (check (ref v "path") => "models/kimi-coding.json")
  (check (ref v "exists") => #t)
  (check (string-contains (ref v "content") "Kimi Coding") => #t)
) ;let*

(let* ((st (pi-config-state (string-append FIXTURE "/config-tui") :env '()))
       (v (pi-config-file-view st (pi-config-cursor (p-index st "kimi-coding") 0) 1))
      ) ;
  (check (ref v "kind") => "model")
  (check (ref v "path") => "models/kimi-coding/k3.1-pro.json")
  (check (ref v "exists") => #t)
) ;let*

(check-report)
