;; resolve-tool-import-path
;; 将库名解析为 tools 内部源文件路径
;;
;; 语法
;; ----
;; (resolve-tool-import-path lib-name current-file)
;;
;; 参数
;; ----
;; lib-name : list?，库名符号列表，如 (liii goldtest-cache)
;; current-file : string?，发起 import 的文件路径
;;
;; 返回值
;; ------
;; tools 内部源文件相对路径（string?）；非工具库返回 #f

(import (scheme base) (liii check) (liii string) (liii goldtest-cache))

(check-set-mode! 'report-failed)

;; ===== 场景1: 当前工具目录下的库直接命中 =====
;; 当前工作目录是 tools/test，liii/goldtest-cache.scm 直接命中
(check
  (resolve-tool-import-path '(liii goldtest-cache) "tests/liii/goldtest-cache/main-test.scm")
  => "liii/goldtest-cache.scm")

;; ===== 场景2: 标准库（如 scheme base）按 load-path 解析，不存在库返回 #f =====
(define resolved-base (resolve-tool-import-path '(scheme base) "tests/liii/x.scm"))
(check-true (and (string? resolved-base) (string-ends? resolved-base "r7rs/scheme/base.scm")))
(check-false (resolve-tool-import-path '(nonexistent lib xyz) "tests/liii/x.scm"))

;; ===== 场景3: 是过程 =====
(check-true (procedure? resolve-tool-import-path))

(check-report)
