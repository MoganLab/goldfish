;; pi-config-aio-modal-slot-picker
;; 构造为指定链条添加槽位的供应商/模型两级选择覆盖层状态。
;;
;; 语法
;; ----
;; (pi-config-aio-modal-slot-picker st chain-id)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图模型。
;; chain-id : string
;; 目标链条 id。
;;
;; 返回值
;; ------
;; list
;; 覆盖层状态 alist。

(import (scheme base)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii pi code modes config state)
  (liii pi code modes config aio-modal)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path-join (path-temp-dir)
    (string-append "pi-cfg-aio-picker-" (number->string (getpid)))
  ) ;path-join
) ;define

(when (file-exists? (path->string TMP-DIR))
  (rmtree (path->string TMP-DIR))
) ;when
(path-mkdir (path-join TMP-DIR "models") :parents #t :exist-ok #t)
(path-write-text (path-join TMP-DIR "models" "providers.json") "[\"google\"]")

(let* ((st (pi-config-state (path->string TMP-DIR) :env '()))
       (m (pi-config-aio-modal-slot-picker st "coding-fast"))
      ) ;
  (check (cdr (assoc 'mode m)) => 'pick-provider)
  (check (cdr (assoc 'chain-id m)) => "coding-fast")
  (check (length (cdr (assoc 'targets m))) => 1)
) ;let*

(rmtree (path->string TMP-DIR))

(check-report)
