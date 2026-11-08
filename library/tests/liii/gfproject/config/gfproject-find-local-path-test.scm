;; gfproject-find-local-path
;; 查找项目根目录下的 gfproject.json 路径。
;;
;; 语法
;; ----
;; (gfproject-find-local-path)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; string 或 #f
;; 本地 gfproject.json 的路径；不在项目中或文件不存在时为 #f。
;;
;; 说明
;; ----
;; 路径固定为 <项目根目录>/gfproject.json。

(import (liii check) (liii gfproject config) (liii string))

(check-set-mode! 'report-failed)

(let ((path (gfproject-find-local-path)))
  (if path
    (begin
      (check-true (string? path))
      (check-true (string-ends? path "gfproject.json"))
    ) ;begin
    (check-true #t)
  ) ;if
) ;let

(check-report)
