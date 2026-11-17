;; pi-config-aio-modal-step
;; 处理 all-in-one 交互覆盖层的按键步进。
;;
;; 语法
;; ----
;; (pi-config-aio-modal-step st modal key)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;; modal : list
;; 当前覆盖层状态。
;; key : (or symbol string)
;; 按键事件。
;;
;; 返回值
;; ------
;; list
;; 包含 modal、writes、message、select-chain 的 alist。

(import (scheme base)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi code modes config state)
  (liii pi code modes config aio-modal)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path-join (path-temp-dir)
    (string-append "pi-cfg-aio-step-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir (path-join TMP-DIR "models") :parents #t :exist-ok #t)
(path-write-text (path-join TMP-DIR "models" "providers.json") "[\"google\"]")
(path-write-text (path-join TMP-DIR "models" "google.json")
  "{\"models\":[\"gemini-3.8-flash\"]}"
) ;path-write-text

(define st (pi-config-state (path->string TMP-DIR) :env '()))

;; 1. new-chain 输入字符后按 Enter 生成写入计划
(let* ((m0 (pi-config-aio-modal-new-chain))
       (r1 (pi-config-aio-modal-step st m0 "c"))
       (r2 (pi-config-aio-modal-step st (cdr (assoc 'modal r1)) "1"))
       (r3 (pi-config-aio-modal-step st (cdr (assoc 'modal r2)) 'enter))
      ) ;
  (check (cdr (assoc 'modal r3)) => #f)
  (check (pair? (cdr (assoc 'writes r3))) => #t)
  (check
    (string-contains? (cdar (cdr (assoc 'writes r3))) "\"id\": \"c1\"")
    =>
    #t
  ) ;check
) ;let*

;; 2. slot-picker 三级选择：先选 provider，再选 model（支持思考时进入 pick-thinking），最后确认写入
(let* ((m0 (pi-config-aio-modal-slot-picker st "coding-fast"))
       (r1 (pi-config-aio-modal-step st m0 'enter))
       (m1 (cdr (assoc 'modal r1)))
       (r2 (pi-config-aio-modal-step st m1 'enter))
       (m2 (cdr (assoc 'modal r2)))
       (r3 (pi-config-aio-modal-step st m2 'down))
       (r4 (pi-config-aio-modal-step st (cdr (assoc 'modal r3)) 'enter))
      ) ;
  (check (cdr (assoc 'mode m1)) => 'pick-model)
  (check (cdr (assoc 'mode m2)) => 'pick-thinking)
  (check (cdr (assoc 'modal r4)) => #f)
  (check (pair? (cdr (assoc 'writes r4))) => #t)
  (check (string-contains? (cdar (cdr (assoc 'writes r4))) "gemini-3.8-flash") => #t)
  (check (string-contains? (cdar (cdr (assoc 'writes r4))) "\"thinkingLevel\": \"high\"") => #t)
) ;let*

;; 3. thinking-picker 调整已有槽位思考强度
(let* ((m0 (pi-config-aio-modal-thinking-picker "coding-fast" 0 "google" "gemini-3.8-flash" "medium"))
       (r1 (pi-config-aio-modal-step st m0 'up))
       (r2 (pi-config-aio-modal-step st (cdr (assoc 'modal r1)) 'enter))
      ) ;
  (check (cdr (assoc 'modal r2)) => #f)
  (check (string-contains? (cdar (cdr (assoc 'writes r2))) "\"thinkingLevel\": \"low\"") => #t)
) ;let*

(rmtree (path->string TMP-DIR))

(check-report)
