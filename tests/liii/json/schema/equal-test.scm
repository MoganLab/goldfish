;; (liii json schema equal) 模块介绍与用法示例
;;
;; (liii json schema equal) 实现 JSON 语义下的相等判定，供 const、enum
;; 与 uniqueItems 等关键字复用。
;;
;; 下方 const/enum/uniqueItems 的断言暂存于此，
;; 后续分别拆分到 keywords-basic 与 keywords-array 的按函数测试文件。

(import (liii json schema equal) (liii json schema) (liii check))

(check-set-mode! 'report-failed)

;; ==== 模块概述 ====
;;
;; - json-equal? - 按 JSON 语义比较两个值是否相等
;;
;; 相等判定遵循 JSON 而非 Scheme：数字按数值比较，布尔与数字严格区分，
;; 对象按无序映射比较，数组按有序序列比较。

;; ==== 如何查看函数的文档和用例 ====
;;
;; 查看模块文档：
;;   gf doc liii/json/schema/equal
;;
;; 查看具体函数文档：
;;   gf doc "json-equal?"

;; ==== 常见用法示例 ===;

;; 示例1：数字按数值比较
(json-equal? 1 1.0) ; => #t

;; 示例2：布尔与数字严格区分
(json-equal? #t 1) ; => #f

;; 示例3：对象是无序映射
(json-equal? '(("a" . 1) ("b" . 2)) '(("b" . 2) ("a" . 1))) ; => #t

;; ==== 暂存的集成断言 ====

;; const 和 enum 使用完全相同的递归相等判定。
(check-true (json-schema-valid? 1 '(("const" . 1.0))))
(check-false (json-schema-valid? #t '(("const" . 1))))
(check-true
  (json-schema-valid? '(("a" . 1) ("b" . 2)) '(("const" ("b" . 2) ("a" . 1))))
) ;check-true
(check-true (json-schema-valid? #(#t 1) '(("enum" . #(#(1 #t) #(#t 1))))))
(check-false (json-schema-valid? #t '(("enum" . #(0 1 null)))))

;; uniqueItems 同样区分嵌套的布尔与数字。
(check-true (json-schema-valid? #(#t 1 #f 0) '(("uniqueItems" . #t))))
(check-false (json-schema-valid? #(1 1.0) '(("uniqueItems" . #t))))
(check-true (json-schema-valid? #((("x" . #t)) (("x" . 1))) '(("uniqueItems"
                                                               . #t)))
) ;check-true
(check-false (json-schema-valid? #((("a" . 1) ("b" . 2)) (("b" . 2) ("a" . 1)))
               '(("uniqueItems" . #t))
             ) ;json-schema-valid?
) ;check-false
(check-true (json-schema-valid? #(1 1) '(("uniqueItems" . #f))))

(check-report)
