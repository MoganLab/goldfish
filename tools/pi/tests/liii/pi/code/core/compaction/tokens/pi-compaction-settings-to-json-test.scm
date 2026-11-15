;; pi-compaction-settings->json
;; 将 <pi-compaction-settings> record 转换为 Scheme-JSON 对象（alist）。
;;
;; 语法
;; ----
;; (pi-compaction-settings->json settings) -> alist
;;
;; 参数
;; ----
;; settings : pi-compaction-settings? | alist
;; 压缩配置 record 或已是 JSON alist。
;;
;; 返回值
;; ----
;; alist
;; 可由 json->string 序列化的 JSON 对象，且可被 json->pi-compaction-settings 往返还原。

(import (scheme base)
  (liii check)
  (liii json)
  (liii pi code core compaction tokens)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? pi-compaction-settings->json))

(let* ((orig (make-pi-compaction-settings #f 8000 9000))
       (j (pi-compaction-settings->json orig))
       (rt (json->pi-compaction-settings (string->json (json->string j))))
      ) ;
  (check (cdr (assoc "enabled" j)) => 'false)
  (check (cdr (assoc "reserveTokens" j)) => 8000)
  (check (cdr (assoc "keepRecentTokens" j)) => 9000)
  (check-true (pi-compaction-settings? rt))
  (check (pi-compaction-settings-enabled rt) => #f)
  (check (pi-compaction-settings-reserve-tokens rt) => 8000)
  (check (pi-compaction-settings-keep-recent-tokens rt) => 9000)
) ;let*

(let* ((orig (pi-compaction-default-settings))
       (j (pi-compaction-settings->json orig))
      ) ;
  (check (cdr (assoc "enabled" j)) => 'true)
  (check (cdr (assoc "reserveTokens" j)) => 16384)
  (check (cdr (assoc "keepRecentTokens" j)) => 20000)
) ;let*

(check-report)
