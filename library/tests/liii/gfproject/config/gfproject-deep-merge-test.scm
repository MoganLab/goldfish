;; gfproject-deep-merge
;; 深度合并两个配置对象。
;;
;; 语法
;; ----
;; (gfproject-deep-merge base overlay)
;;
;; 参数
;; ----
;; base : list
;; 基础配置对象。
;; overlay : list
;; 覆盖配置对象。
;;
;; 返回值
;; ----
;; list
;; 合并后的配置对象；overlay 中的同名键覆盖 base，
;; 两侧都有的对象键会递归合并。
;;
;; 说明
;; ----
;; 空对象 (()) 与空列表 '() 语义不同：
;; 前者是「没有内容」，后者是「覆盖为空」。

(import (liii check) (liii gfproject config) (liii json))

(check-set-mode! 'report-failed)

;; 不同键：两者都保留
(let ((merged (gfproject-deep-merge (string->json "{\"a\": 1}")
                  (string->json "{\"b\": 2}")
             ))) ;
  (check-true (json-object? merged))
  (check (json-ref merged "a") => 1)
  (check (json-ref merged "b") => 2)
) ;let*

;; 同名键：overlay 覆盖 base
(let ((merged (gfproject-deep-merge (string->json "{\"a\": 1}")
                  (string->json "{\"a\": 2}")
             ))) ;
  (check (json-ref merged "a") => 2)
) ;let*

;; 同名对象键：递归合并
(let ((merged (gfproject-deep-merge (string->json "{\"a\": {\"x\": 1}}")
                  (string->json "{\"a\": {\"y\": 2}}")
             ))) ;
  (check (json-ref merged "a") => '(("y" . 2) ("x" . 1)))
) ;let*

;; base 为空对象时直接返回 overlay
(check (gfproject-deep-merge (string->json "{}") (string->json "{\"a\": 1}"))
  => '(("a" . 1)))

;; overlay 为空列表时直接返回 overlay（空列表）
(check (gfproject-deep-merge (string->json "{\"a\": 1}") '()) => '())

(check-report)
