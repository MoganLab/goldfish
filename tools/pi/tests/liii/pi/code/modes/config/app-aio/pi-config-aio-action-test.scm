;; pi-config-aio-action
;; 判定 all-in-one 子标签页按键触发的交互动作。
;;
;; 语法
;; ----
;; (pi-config-aio-action st cursor active key)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;; cursor : list
;; 当前光标。
;; active : integer
;; 激活列。
;; key : (or symbol string)
;; 按键事件。
;;
;; 返回值
;; ------
;; (or symbol pair #f)
;; 'new-chain、(add-slot . chain-id)、'set-default 或 #f。

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

;; 1. 左列按 a 触发 new-chain；在 + new chain... 处按 enter 触发 new-chain
(check (pi-config-aio-action st '((tab . 1) (chain . 0) (node . 0)) 0 "a")
  =>
  'new-chain
) ;check
(check (pi-config-aio-action st '((tab . 1) (chain . 2) (node . 0)) 0 'enter)
  =>
  'new-chain
) ;check

;; 2. 左列在真实链条处按 enter 触发 set-default
(check (pi-config-aio-action st '((tab . 1) (chain . 0) (node . 0)) 0 'enter)
  =>
  'set-default
) ;check

;; 3. 中列按 a 或在 + add slot... 处按 enter 触发 add-slot
(check (pi-config-aio-action st '((tab . 1) (chain . 0) (node . 0)) 1 "a")
  =>
  '(add-slot . "coding-fast")
) ;check
(check (pi-config-aio-action st '((tab . 1) (chain . 0) (node . 3)) 1 'enter)
  =>
  '(add-slot . "coding-fast")
) ;check

;; 4. 删除链条、删除槽位、调整槽位思考强度
(check (pi-config-aio-action st '((tab . 1) (chain . 0) (node . 0)) 0 "d")
  =>
  '(delete-chain . "coding-fast")
) ;check
(check (pi-config-aio-action st '((tab . 1) (chain . 0) (node . 1)) 1 "d")
  =>
  '(delete-slot "coding-fast" 1)
) ;check
(check (car (pi-config-aio-action st '((tab . 1) (chain . 0) (node . 0)) 1 "t"))
  =>
  'edit-thinking
) ;check
(check (pi-config-aio-action st '((tab . 1) (chain . 0) (node . 1)) 1 " ")
  =>
  '(promote-slot "coding-fast" 1)
) ;check

(check-report)
