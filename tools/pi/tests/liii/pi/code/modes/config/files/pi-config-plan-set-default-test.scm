;; pi-config-plan-set-default
;; 计算把选中模型设为默认模型时需要写入 settings.json 的计划（纯函数）。
;;
;; 语法
;; ----
;; (pi-config-plan-set-default st cursor . opt-column)
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
;; 包含 ok、message 与 writes 的写入计划。

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
  (let ((pair (assoc key alist))) (and pair (cdr pair)))
) ;define

(define (p-index st id)
  (list-index (lambda (p) (string=? (pi-config-provider-id p) id))
    (pi-config-state-providers st)
  ) ;list-index
) ;define

(let* ((st (pi-config-state (string-append FIXTURE "/config-tui") :env '()))
       (plan (pi-config-plan-set-default st
               (pi-config-cursor (p-index st "google") 0)
               1
             ) ;pi-config-plan-set-default
       ) ;plan
       (write (assoc "settings.json" (ref plan "writes")))
       (text (and write (cdr write)))
      ) ;
  (check (ref plan "ok") => #t)
  (check (string-contains text "\"defaultProvider\": \"google\"") => #t)
) ;let*

;; provider 列按 Enter 不设默认
(let* ((st (pi-config-state (string-append FIXTURE "/config-tui") :env '()))
       (plan (pi-config-plan-set-default st
               (pi-config-cursor (p-index st "google") 0)
               0
             ) ;pi-config-plan-set-default
       ) ;plan
      ) ;
  (check (ref plan "ok") => #f)
) ;let*

(check-report)
