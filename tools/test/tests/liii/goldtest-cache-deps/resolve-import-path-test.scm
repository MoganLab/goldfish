;; resolve-import-path
;; 按真实加载路径将库名解析为 .scm 源文件路径
;;
;; 语法
;; ----
;; (resolve-import-path lib-name current-file)
;;
;; 参数
;; ----
;; lib-name : list?，库名符号列表，如 (liii goldtest-cache)
;; current-file : string?，发起 import 的文件路径
;;
;; 返回值
;; ------
;; 源文件相对或绝对路径（string?）；C 内建库或不存在库返回 #f

(import (scheme base) (liii check) (liii string) (liii goldtest-cache-deps))

(check-set-mode! 'report-failed)

;; ===== 场景1: 当前目录下的库直接命中 =====
(check
  (resolve-import-path '(liii goldtest-cache) "tests/liii/goldtest-cache/main-test.scm")
  => "liii/goldtest-cache.scm")

;; ===== 场景2: 标准库按 gf_lib 加载路径命中 =====
(define resolved-base (resolve-import-path '(scheme base) "tests/liii/x.scm"))
(check-true (and (string? resolved-base) (string-ends? resolved-base "r7rs/scheme/base.scm")))

;; ===== 场景3: 不存在库返回 #f =====
(check-false (resolve-import-path '(nonexistent lib xyz) "tests/liii/x.scm"))

;; ===== 场景4: 是过程 =====
(check-true (procedure? resolve-import-path))

(check-report)
