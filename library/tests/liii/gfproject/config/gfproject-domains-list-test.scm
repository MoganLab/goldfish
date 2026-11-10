;; gfproject-domains-list
;; 获取规范化的 domain 列表，每个 domain 包含 name、root、depends-on、tests、hint 五个字段。
;;
;; 语法
;; ----
;; (gfproject-domains-list)
;; (gfproject-domains-list gf-lib)
;;
;; 参数
;; ----
;; gf-lib : string (可选)
;; 标准库目录路径；缺省时使用默认标准库目录。
;;
;; 返回值
;; ----
;; list of alist
;; 域列表，每项形如 ((name . str) (root . str) (depends-on . list) (tests . str) (hint . str))。

(import (liii check) (liii gfproject config) (liii list))

(check-set-mode! 'report-failed)

;; 本项目包含 r7rs、goldfish、library 等域
(let ((ds (gfproject-domains-list)))
  (check-true (pair? ds))
  (let ((names (map (lambda (d) (cdr (assq 'name d))) ds)))
    (check-true (not (not (member "r7rs" names string=?))))
    (check-true (not (not (member "goldfish" names string=?))))
    (check-true (not (not (member "library" names string=?))))
  ) ;let
  ;; 验证 goldfish 域的字段
  (let* ((gf (find (lambda (d) (string=? (cdr (assq 'name d)) "goldfish")) ds)))
    (check-true (not (not gf)))
    (check (cdr (assq 'root gf)) => "goldfish/")
    (check (cdr (assq 'tests gf)) => "goldfish/tests")
    (check (cdr (assq 'hint gf)) => "goldfish/hint")
    (check (cdr (assq 'depends-on gf)) => '("r7rs"))
  ) ;let*
) ;let

(check-report)
