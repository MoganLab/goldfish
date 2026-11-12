;; test-cache-record!
;; 按退出码记录缓存：仅 exit-code=0 持久化，失败绝不缓存并清除既有条目
;;
;; 语法
;; ----
;; (test-cache-record! path exit-code)
;;
;; 参数
;; ----
;; path : string?，缓存文件路径
;; exit-code : integer?，测试子进程退出码
;;
;; 返回值
;; ------
;; unspecified

(import (scheme base) (scheme file) (liii check) (liii goldtest-cache))

(check-set-mode! 'report-failed)

;; ===== 场景1: exit-code=0 记录缓存（SUCCESSFUL 原则） =====
(define key "5d867f4ff816b2ff84e97aa7b8fbf7ee53adf263e63f9ca19acff4e80078d31f")
(define ok-path (test-cache-path (string-append key "01")))
(test-cache-record! ok-path 0)
(check-true (file-exists? ok-path))
(delete-file ok-path)

;; ===== 场景2: exit-code!=0 绝不写缓存 =====
(define fail-path (test-cache-path (string-append key "02")))
(test-cache-record! fail-path 1)
(check-false (file-exists? fail-path))

;; ===== 场景3: 失败后清除既有条目（防止陈旧成功缓存蒙蔽） =====
(test-cache-record! fail-path 0)
(check-true (file-exists? fail-path))
(test-cache-record! fail-path 1)
(check-false (file-exists? fail-path))

;; ===== 场景4: 是过程 =====
(check-true (procedure? test-cache-record!))

(check-report)
