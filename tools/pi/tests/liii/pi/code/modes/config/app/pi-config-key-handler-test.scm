;; pi-config-key-handler
;; 处理配置 TUI 的按键，返回新的光标位置与动作。
;;
;; 语法
;; ----
;; (pi-config-key-handler st cursor active key)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;;
;; cursor : list
;; 当前光标位置。
;;
;; active : integer
;; 激活列编号。
;;
;; key : (or symbol string)
;; 按键事件。
;;
;; 返回值
;; ----
;; list
;; 包含新光标与动作的结果列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii pi code modes config state)
  (liii pi code modes config layout)
  (liii pi code modes config app)
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

(define providers (pi-config-state-providers st))

(define n (length providers))

;; 1. 模型列向下导航不会被前一个 provider 的模型数夹断
(let* ((target-p (list-ref providers (- n 2)))
       (prev-p (list-ref providers (- n 3)))
       (r (pi-config-key-handler st (pi-config-cursor (- n 2) 3) 1 'down))
      ) ;
  (check (cdr (assoc 'model (car r))) => 4)
  (check (cdr (assoc 'provider (car r))) => (- n 2))
  ;; 夹具形状：目标 provider 的模型数多于前一个 provider
  (check (< (length (pi-config-provider-models prev-p))
           (length (pi-config-provider-models target-p))
         ) ;<
    =>
    #t
  ) ;check
) ;let*

;; 2. G 直达目标 provider 的最后一个模型
(let* ((target-p (list-ref providers (- n 2)))
       (r (pi-config-key-handler st (pi-config-cursor (- n 2) 0) 1 "G"))
      ) ;
  (check (cdr (assoc 'model (car r)))
    =>
    (- (length (pi-config-provider-models target-p)) 1)
  ) ;check
) ;let*

(check-report)
