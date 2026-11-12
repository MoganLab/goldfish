;; compute-tools-deps-hash
;; 对依赖列表中的文件内容计算确定性串联指纹
;;
;; 语法
;; ----
;; (compute-tools-deps-hash deps)
;;
;; 参数
;; ----
;; deps : list?，依赖文件路径列表
;;
;; 返回值
;; ------
;; string?，64 字符十六进制 SHA-256

(import (scheme base) (liii check) (liii string) (liii goldtest-cache))

(check-set-mode! 'report-failed)

;; ===== 场景1: 空依赖列表哈希稳定且为 64 位十六进制 =====
(check (compute-tools-deps-hash '()) => (compute-tools-deps-hash '()))
(check-true (= 64 (string-length (compute-tools-deps-hash '()))))

;; ===== 场景2: 相同依赖列表哈希确定 =====
(define d1 (list "liii/goldtest-cache.scm"))
(check (compute-tools-deps-hash d1) => (compute-tools-deps-hash d1))

;; ===== 场景3: 依赖顺序不同则哈希不同（拼接顺序参与指纹） =====
(define d2 (list "liii/goldtest-cache.scm" "liii/goldtest-args.scm"))
(define d3 (list "liii/goldtest-args.scm" "liii/goldtest-cache.scm"))
(check-false (string=? (compute-tools-deps-hash d2) (compute-tools-deps-hash d3)))

;; ===== 场景4: 是过程 =====
(check-true (procedure? compute-tools-deps-hash))

(check-report)
