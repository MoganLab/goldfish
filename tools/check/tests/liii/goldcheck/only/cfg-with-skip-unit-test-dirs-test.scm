;; cfg-with-skip-unit-test-dirs
;; 为检查配置更新或添加 skip-unit-test-dirs 选项。
;;
;; 语法
;; ----
;; (cfg-with-skip-unit-test-dirs cfg val)
;;
;; 参数
;; ----
;; cfg : list          原始配置（alist）
;; val : boolean       是否跳过目录级单元测试规范检查
;;
;; 返回值
;; ------
;; list
;; 包含最新 ("skip-unit-test-dirs" . val) 的配置 alist。

(import (scheme base) (liii check) (liii list) (liii goldcheck only))

(check-set-mode! 'report-failed)

;; 1. cfg 为空列表时添加该项
(check (cfg-with-skip-unit-test-dirs '() #t)
  =>
  '(("skip-unit-test-dirs" . #t))
) ;check

;; 2. cfg 为非列表对象时当作空列表处理
(check (cfg-with-skip-unit-test-dirs #f #t)
  =>
  '(("skip-unit-test-dirs" . #t))
) ;check

;; 3. 原配置已有该项时覆盖原有值
(let ((new-cfg (cfg-with-skip-unit-test-dirs
                 '(("rules" . ()) ("skip-unit-test-dirs" . #f))
                 #t
               ) ;cfg-with-skip-unit-test-dirs
      ) ;new-cfg
     ) ;
  (check (cdr (assoc "skip-unit-test-dirs" new-cfg)) => #t)
  (check
    (length
      (filter (lambda (kv) (string=? (car kv) "skip-unit-test-dirs")) new-cfg)
    ) ;length
    =>
    1
  ) ;check
) ;let

(check-report)
