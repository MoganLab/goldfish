;; all-in-one-cfg-chains
;; 获取链定义列表。
;;
;; 语法
;; ----
;; (all-in-one-cfg-chains cfg)
;;
;; 参数
;; ----
;; cfg : <all-in-one-cfg>
;; 配置记录。
;;
;; 返回值
;; ----
;; list of <chain-def>
;; 链定义记录列表。

(import (scheme base) (liii check) (liii pi code all-in-one types))

(check-set-mode! 'report-failed)

(let* ((c1 (make-chain-def "c1" "链1" '()))
       (cfg (make-all-in-one-cfg #t 1000 100 50 (list c1)))
      ) ;
  (check (length (all-in-one-cfg-chains cfg)) => 1)
  (check (chain-def-id (car (all-in-one-cfg-chains cfg))) => "c1")
) ;let*

(check-report)
