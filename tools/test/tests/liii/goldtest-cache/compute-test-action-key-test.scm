;; compute-test-action-key
;; 纯函数：按四因子（解释器/测试文件/依赖/参数哈希）联合计算 Cache Key
;;
;; 语法
;; ----
;; (compute-test-action-key bin-hash test-file-hash tools-deps-hash argv-flags)
;;
;; 参数
;; ----
;; bin-hash : string?，解释器二进制 SHA-256
;; test-file-hash : string?，测试文件内容 SHA-256
;; tools-deps-hash : string?，依赖指纹 SHA-256
;; argv-flags : string?，执行参数（如 -m r7rs -I tools/pi）
;;
;; 返回值
;; ------
;; string?，64 字符十六进制 SHA-256；任一因子变更则 Key 变更

(import (scheme base) (liii check) (liii string) (liii goldtest-cache))

(check-set-mode! 'report-failed)

;; ===== 场景1: 相同输入产生相同 Key =====
(define k1 (compute-test-action-key "aaa" "bbb" "ccc" "ddd"))
(check k1 => (compute-test-action-key "aaa" "bbb" "ccc" "ddd"))

;; ===== 场景2: 任一因子变更 Key 随之变更 =====
(check-false (string=? k1 (compute-test-action-key "aaa2" "bbb" "ccc" "ddd")))
(check-false (string=? k1 (compute-test-action-key "aaa" "bbb2" "ccc" "ddd")))
(check-false (string=? k1 (compute-test-action-key "aaa" "bbb" "ccc2" "ddd")))
(check-false (string=? k1 (compute-test-action-key "aaa" "bbb" "ccc" "ddd2")))

;; ===== 场景3: Key 是 64 字符十六进制 =====
(check-true (= 64 (string-length k1)))

;; ===== 场景4: 是过程 =====
(check-true (procedure? compute-test-action-key))

(check-report)
