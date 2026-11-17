;; pi-config-aio-modal-handle
;; 处理 all-in-one 交互覆盖层按键并执行写入。
;;
;; 语法
;; ----
;; (pi-config-aio-modal-handle st modal key)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;; modal : list
;; 覆盖层状态。
;; key : (or symbol string)
;; 按键事件。
;;
;; 返回值
;; ------
;; list
;; (next-modal message changed?)。

(import (scheme base)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii tui)
  (liii pi code modes config state)
  (liii pi code modes config app)
  (liii pi code modes config aio-modal)
  (liii pi code modes config app-aio)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path-join (path-temp-dir)
    (string-append "pi-cfg-aio-mhandle-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

(let* ((st (pi-config-state (path->string TMP-DIR) :env '()))
       (m0 (pi-config-aio-modal-new-chain))
       (r1 (pi-config-aio-modal-handle st m0 "x"))
       (r2 (pi-config-aio-modal-handle st (car r1) 'enter))
      ) ;
  (check (caddr r1) => #f)
  (check (car r2) => #f)
  (check (caddr r2) => #t)
  (check (string-contains? (cadr r2) "created chain x") => #t)
  (check (file-exists? (path->string (path-join TMP-DIR "all-in-one.json")))
    =>
    #t
  ) ;check
) ;let*

(rmtree (path->string TMP-DIR))

;; 端到端：在 all-in-one 子标签页创建新伪模型、添加槽位、调整思考强度、置顶与删除
(define (make-e2e-dir name)
  (let ((d (path-join (path-temp-dir) (string-append "pi-cfg-e2e-" name "-" (number->string (getpid))))))
    (when (file-exists? (path->string d)) (rmtree (path->string d)))
    (path-mkdir (path-join d "models") :parents #t :exist-ok #t)
    (path-write-text (path-join d "models" "providers.json") "[\"kimi-coding\",\"google\",\"minimax-cn\"]")
    (path->string d)
  ) ;let
) ;define

(let ((dir (make-e2e-dir "create-and-slot")) (vt (make-virtual-terminal :width 120 :height 24)))
  (for-each (lambda (k) (virtual-terminal-push-key! vt k))
    (list "2" "a" "m" "y" "-" "c" "h" 'enter 'down 'down 'right 'enter 'enter 'enter 'enter
      "1" "g" 'down 'down 'down 'q
    ) ;list
  ) ;for-each
  (check (pi-config-run dir :terminal vt :env '()) => 'chat)
  (check (string-contains? (path-read-text (path-join dir "all-in-one.json")) "\"id\": \"my-ch\"") => #t)
  (check (string-contains? (string-join (virtual-terminal-screen vt) "\n") "my-ch") => #t)
  (rmtree dir)
) ;let

(let ((dir (make-e2e-dir "edit-del-promote")) (vt (make-virtual-terminal :width 120 :height 24)))
  (for-each (lambda (k) (virtual-terminal-push-key! vt k))
    (list "2" 'right 'down " " "t" 'down 'enter "d" 'left "d" 'q)
  ) ;for-each
  (check (pi-config-run dir :terminal vt :env '()) => 'chat)
  (let ((aio-json (path-read-text (path-join dir "all-in-one.json"))))
    (check (string-contains? aio-json "\"coding-fast\"") => #f)
    (check (string-contains? aio-json "\"coding-high-quality\"") => #t)
  ) ;let
  (rmtree dir)
) ;let

(check-report)
