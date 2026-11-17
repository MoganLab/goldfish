;; pi-config-aio-add-slot
;; 生成向指定 all-in-one 链条追加新槽位的写入计划。
;;
;; 语法
;; ----
;; (pi-config-aio-add-slot st chain-id provider-id model-id thinking-level)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;; chain-id : string
;; 目标链条 id。
;; provider-id : string
;; 供应商 id。
;; model-id : string
;; 模型 id。
;; thinking-level : string
;; 思考等级。
;;
;; 返回值
;; ------
;; list
;; 包含 ok、message、writes 的 alist。

(import (scheme base)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi code modes config state)
  (liii pi code modes config aio-writes)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path-join (path-temp-dir)
    (string-append "pi-cfg-aio-add-slot-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

(let ((st (pi-config-state (path->string TMP-DIR) :env '())))
  ;; 1. 不存在的链条
  (let ((r0 (pi-config-aio-add-slot st "missing" "google" "gemini-3.8-flash" "medium"))
       ) ;
    (check (cdr (assoc "ok" r0)) => #f)
  ) ;let
  ;; 2. 向 coding-fast 追加槽位
  (let* ((r1 (pi-config-aio-add-slot st "coding-fast" "google" "gemini-3.8-pro" "high"))
         (writes (cdr (assoc "writes" r1)))
        ) ;
    (check (cdr (assoc "ok" r1)) => #t)
    (check (caar writes) => "all-in-one.json")
    (check (string-contains? (cdar writes) "gemini-3.8-pro") => #t)
  ) ;let*
) ;let

(rmtree (path->string TMP-DIR))

(check-report)
