;; pi-config-aio-key-handler
;; 处理 all-in-one 子标签页的链条与节点导航按键。
;;
;; 语法
;; ----
;; (pi-config-aio-key-handler st cursor active key)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;; cursor : list
;; 当前光标。
;; active : integer
;; 激活列编号。
;; key : (or symbol string)
;; 按键事件。
;;
;; 返回值
;; ------
;; list
;; (新光标 新激活列 是否退出)。

(import (scheme base)
  (liii check)
  (liii pi code modes config state)
  (liii pi code modes config app-aio)
) ;import

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(define st (pi-config-state (string-append FIXTURE "/config-tui") :env '()))

;; 1. 链条列下移：chain 从 0 -> 1，node 重置为 0
(let* ((c0 '((provider . 0) (model . 0) (tab . 1) (chain . 0) (node . 2)))
       (r (pi-config-aio-key-handler st c0 0 'down))
      ) ;
  (check (cdr (assoc 'chain (car r))) => 1)
  (check (cdr (assoc 'node (car r))) => 0)
  (check (cadr r) => 0)
) ;let*

;; 2. 进入节点列并下移节点
(let* ((c0 '((provider . 0) (model . 0) (tab . 1) (chain . 0) (node . 0)))
       (r1 (pi-config-aio-key-handler st c0 0 'right))
       (r2 (pi-config-aio-key-handler st (car r1) (cadr r1) 'down))
      ) ;
  (check (cadr r1) => 1)
  (check (cdr (assoc 'node (car r2))) => 1)
) ;let*

;; 3. G 跳到最后一项（含 + new chain... 与 + add slot...）
(let* ((c0 '((provider . 0) (model . 0) (tab . 1) (chain . 0) (node . 0)))
       (r1 (pi-config-aio-key-handler st c0 0 "G"))
       (r2 (pi-config-aio-key-handler st c0 1 "G"))
      ) ;
  (check (cdr (assoc 'chain (car r1))) => 2)
  (check (cdr (assoc 'node (car r2))) => 3)
) ;let*

(check-report)
