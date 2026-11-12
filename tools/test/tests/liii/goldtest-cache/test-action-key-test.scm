;; test-action-key
;; 综合计算单测试文件的 Action Cache Key（解释器+测试文件+依赖+参数）
;;
;; 语法
;; ----
;; (test-action-key test-file . maybe-flags)
;;
;; 参数
;; ----
;; test-file : string?，测试文件路径
;; maybe-flags : 可选 string?，执行参数
;;
;; 返回值
;; ------
;; string?，64 字符十六进制 SHA-256

(import (scheme base) (liii check) (liii string) (liii goldtest-cache))

(check-set-mode! 'report-failed)

;; ===== 场景1: 同一测试文件两次计算 Key 一致 =====
(define tf "tests/liii/goldtest-cache/main-test.scm")
(check (test-action-key tf) => (test-action-key tf))

;; ===== 场景2: 附加 flags 改变 Key =====
(check-false (string=? (test-action-key tf) (test-action-key tf "-m r7rs")))

;; ===== 场景3: Key 为 64 字符十六进制 =====
(check-true (= 64 (string-length (test-action-key tf))))

;; ===== 场景4: 是过程 =====
(check-true (procedure? test-action-key))

(check-report)
