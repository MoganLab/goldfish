;; pi-user-provider-model-list
;; 返回配置视图使用的完整模型列表，可用模型在前并带 enabled 标记。
;;
;; 语法
;; ----
;; (pi-user-provider-model-list config-dir p-id)
;;
;; 参数
;; ----
;; config-dir : string
;; 用户配置目录。
;; p-id : string
;; provider id。
;;
;; 返回值
;; ----
;; list
;; 带有 enabled 标记的模型 alist 列表。

(import (liii check) (liii pi code config user-models))

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(let* ((models (pi-user-provider-model-list (string-append FIXTURE "/full") "kimi-coding")
       ) ;models
       (first (car models))
      ) ;
  (check (pair? models) => #t)
  (check (cdr (assoc "id" first)) => "k3.1-pro")
  (check (cdr (assoc "enabled" first)) => #t)
  ;; 白名单外的内置模型以 enabled #f 出现在后面
  (check (cdr (assoc "enabled" (list-ref models 1))) => #f)
) ;let*

(check-report)
