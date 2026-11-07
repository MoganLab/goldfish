;; pi-config-apply-writes
;; 接受配置目录或 pi-config-state，把写入计划原子落盘。
;;
;; 语法
;; ----
;; (pi-config-apply-writes st writes)
;;
;; 参数
;; ----
;; st : string | pi-config-state
;; 配置目录或配置 TUI 的视图模型。
;; writes : list
;; (相对路径 . 文本) 列表。
;;
;; 返回值
;; ----
;; alist
;; 包含 ok、message 与 failed 的结果。

(import (liii check)
  (liii path)
  (liii shutil)
  (liii pi code modes config json-write)
) ;import

(check-set-mode! 'report-failed)

(define DIR
  (path->string (path-join (path-temp-dir) "pi-config-apply-writes-test"))
) ;define

(when (file-exists? DIR)
  (rmtree DIR #t)
) ;when
(path-mkdir DIR :parents #t :exist-ok #t)

;; 直接传目录字符串
(let ((res (pi-config-apply-writes DIR
             (list (cons "models/providers.json" "[\"google\"]\n"))
           ) ;pi-config-apply-writes
      ) ;res
     ) ;
  (check (cdr (assoc "ok" res)) => #t)
  (check (cdr (assoc "failed" res)) => #f)
) ;let

(check
  (path-read-text (path->string (path-join (path DIR) (path "models/providers.json")))
  ) ;path-read-text
  =>
  "[\"google\"]\n"
) ;check

(rmtree DIR #t)

(check-report)
