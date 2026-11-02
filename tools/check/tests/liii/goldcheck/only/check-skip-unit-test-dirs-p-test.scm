;; check-skip-unit-test-dirs?
;; 判断检查配置中是否指定了跳过目录级单元测试检查。
;;
;; 语法
;; ----
;; (check-skip-unit-test-dirs? cfg)
;;
;; 参数
;; ----
;; cfg : any  检查配置（alist、json-object 或其他对象）
;;
;; 返回值
;; ------
;; boolean
;; 若配置中 skip-unit-test-dirs 为真值则返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii goldcheck only))

(check-set-mode! 'report-failed)

;; 1. cfg 为 #f 时返回 #f
(check (check-skip-unit-test-dirs? #f) => #f)

;; 2. cfg 为空列表时返回 #f
(check (check-skip-unit-test-dirs? '()) => #f)

;; 3. cfg 包含 string key "skip-unit-test-dirs" . #t 时返回 #t
(check (check-skip-unit-test-dirs? '(("skip-unit-test-dirs" . #t))) => #t)

;; 4. cfg 包含 string key "skip-unit-test-dirs" . #f 时返回 #f
(check (check-skip-unit-test-dirs? '(("skip-unit-test-dirs" . #f))) => #f)

;; 5. cfg 包含 symbol key 'skip-unit-test-dirs . #t 时返回 #t
(check (check-skip-unit-test-dirs? '((skip-unit-test-dirs . #t))) => #t)

;; 6. 其他非 pair/json 对象返回 #f
(check (check-skip-unit-test-dirs? 123) => #f)

(check-report)
