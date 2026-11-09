;; gfproject-hint-dirs
;; 获取各 domain 推导出的类型接口（hint）目录列表。
;;
;; 语法
;; ----
;; (gfproject-hint-dirs)
;; (gfproject-hint-dirs gf-lib)
;;
;; 参数
;; ----
;; gf-lib : string (可选)
;; 标准库目录路径；缺省时使用默认标准库目录。
;;
;; 返回值
;; ----
;; list of string
;; 各 domain 的 hint 目录路径列表；hint 目录与 tests 目录同源，
;; 由 domain root 追加 "hint" 段得到。

(import (liii check) (liii gfproject config) (liii list))

(check-set-mode! 'report-failed)

(let ((dirs (gfproject-hint-dirs)))
  (check-true (pair? dirs))
  ;; hint 目录与 domain 一一对应
  (check dirs
    => (map (lambda (d) (cdr (assq 'hint d))) (gfproject-domains-list))
  )
  ;; 主要板块的 hint 目录
  (check-true (not (not (member "r7rs/hint" dirs string=?))))
  (check-true (not (not (member "goldfish/hint" dirs string=?))))
  (check-true (not (not (member "library/hint" dirs string=?))))
) ;let

(check-report)
