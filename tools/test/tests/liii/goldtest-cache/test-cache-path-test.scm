;; test-cache-path
;; 由 Action Key 计算分片缓存文件路径：<base>/<前2位>/<后62位>
;;
;; 语法
;; ----
;; (test-cache-path key)
;;
;; 参数
;; ----
;; key : string?，64 字符十六进制 Action Cache Key
;;
;; 返回值
;; ------
;; string?，缓存文件绝对路径

(import (scheme base) (liii check) (liii string) (liii goldtest-cache))

(check-set-mode! 'report-failed)

;; ===== 场景1: 路径按 <base>/<前2位>/<后62位> 分片 =====
(define key "5d867f4ff816b2ff84e97aa7b8fbf7ee53adf263e63f9ca19acff4e80078d31f")
(define cpath (test-cache-path key))
(check-true (string-contains cpath "/5d/867f4ff816b2ff84e97aa7b8fbf7ee53adf263e63f9ca19acff4e80078d31f"))

;; ===== 场景2: 相同 Key 路径一致，不同 Key 路径不同 =====
(check (test-cache-path key) => (test-cache-path key))
(check-false (string=? cpath (test-cache-path (string-append key "ff"))))

;; ===== 场景3: 是过程 =====
(check-true (procedure? test-cache-path))

(check-report)
