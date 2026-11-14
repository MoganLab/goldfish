;; json->pi-compaction-settings
;; 将 JSON 对象（settings.json 中的 compaction 字段）转换为 <pi-compaction-settings> record。
;;
;; 语法
;; ----
;; (json->pi-compaction-settings obj)
;;
;; 参数
;; ----
;; obj : any?
;; string->json 解析出的 alist，或已是 <pi-compaction-settings> 的 record。
;;
;; 返回值
;; ----
;; pi-compaction-settings?
;; 解析后的压缩配置 record；缺失字段使用 pi-compaction-default-settings 默认值。

(import (liii check)
  (liii json)
  (liii pi code core compaction tokens)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? json->pi-compaction-settings))

;; 1. 完整 JSON 解析（含 JSON 布尔 true/false）
(let* ((j (string->json "{\"enabled\":false,\"reserveTokens\":8000,\"keepRecentTokens\":9000}"))
       (s (json->pi-compaction-settings j))
      ) ;
  (check-true (pi-compaction-settings? s))
  (check (pi-compaction-settings-enabled s) => #f)
  (check (pi-compaction-settings-reserve-tokens s) => 8000)
  (check (pi-compaction-settings-keep-recent-tokens s) => 9000)
) ;let*

;; 2. 空对象与非对象输入：使用默认值
(let ((s1 (json->pi-compaction-settings (string->json "{}")))
      (s2 (json->pi-compaction-settings '()))
      (s3 (json->pi-compaction-settings #f))
     ) ;
  (check-true (pi-compaction-settings? s1))
  (check (pi-compaction-settings-enabled s1) => #t)
  (check (pi-compaction-settings-reserve-tokens s1) => 16384)
  (check (pi-compaction-settings-keep-recent-tokens s1) => 20000)
  (check-true (pi-compaction-settings? s2))
  (check (pi-compaction-settings-enabled s2) => #t)
  (check-true (pi-compaction-settings? s3))
  (check (pi-compaction-settings-enabled s3) => #t)
) ;let

;; 3. 已是 record 原样返回
(let* ((orig (make-pi-compaction-settings #f 4096 10000))
       (out (json->pi-compaction-settings orig))
      ) ;
  (check (eq? out orig) => #t)
) ;let*

(check-report)
