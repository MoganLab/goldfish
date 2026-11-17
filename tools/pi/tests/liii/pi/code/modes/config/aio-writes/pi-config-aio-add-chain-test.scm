;; pi-config-aio-add-chain
;; 生成向 all-in-one.json 追加新伪模型（chain）的写入计划。
;;
;; 语法
;; ----
;; (pi-config-aio-add-chain st chain-id)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;; chain-id : string
;; 新链条 id。
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
    (string-append "pi-cfg-aio-add-chain-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

(let ((st (pi-config-state (path->string TMP-DIR) :env '())))
  ;; 1. 空 id 拒绝
  (let ((r0 (pi-config-aio-add-chain st "  ")))
    (check (cdr (assoc "ok" r0)) => #f)
  ) ;let
  ;; 2. 重复 id 拒绝
  (let ((r1 (pi-config-aio-add-chain st "coding-fast")))
    (check (cdr (assoc "ok" r1)) => #f)
  ) ;let
  ;; 3. 新 id 成功生成 all-in-one.json 写入计划
  (let* ((r2 (pi-config-aio-add-chain st "my-chain"))
         (writes (cdr (assoc "writes" r2)))
        ) ;
    (check (cdr (assoc "ok" r2)) => #t)
    (check (caar writes) => "all-in-one.json")
    (check (string-contains? (cdar writes) "\"id\": \"my-chain\"") => #t)
  ) ;let*
) ;let

(rmtree (path->string TMP-DIR))

(check-report)
