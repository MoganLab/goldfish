;; (liii json schema regex) 模块介绍与用法示例
;;
;; (liii json schema regex) 封装正则表达式后端：当前使用 PCRE（grep -P），
;; 表达式与输入都通过参数传入，不经过 shell。

(import (liii json schema regex) (liii json schema) (liii check))

(check-set-mode! 'report-failed)

;; ==== 模块概述 ====
;;
;; - regex-search? - 用正则表达式在字符串中搜索匹配
;; - regex-valid?  - 判断正则表达式是否可以被后端编译
;;
;; 该函数是“搜索”而非隐式的整串匹配，需要整串匹配请自行使用 ^ 与 $。

;; ==== 如何查看函数的文档和用例 ====
;;
;; 查看模块文档：
;;   gf doc liii/json/schema/regex
;;
;; 查看具体函数文档：
;;   gf doc "regex-search?"

;; ==== 常见用法示例 ===;

;; 示例1：搜索匹配
(regex-search? "abc" "xxabcxx")

;; 示例2：整串匹配需要锚点
(regex-search? "^abc$" "xxabcxx")

;; ==== 暂存的集成断言 ====

;; pattern 关键字只作用于字符串实例。
(check-true (json-schema-valid? "xxabcxx" '(("pattern" . "abc"))))
(check-false (json-schema-valid? "xxabcxx" '(("pattern" . "^abc$"))))
(check-false (json-schema-valid? "xyz" '(("pattern" . "abc"))))
(check-true (json-schema-valid? 42 '(("pattern" . "abc"))))
(check-catch 'json-schema-schema-error
  (json-schema-valid? "x" '(("pattern" . "[")))
) ;check-catch

;; 每个匹配的 patternProperties 分支都会被求值。
(check-true
  (json-schema-valid? '(("alpha" . 1) ("beta" . "ok"))
    '(("patternProperties"
       ("^a" ("type" . "integer"))
       ("pha$" ("type" . "integer"))
       ("^b" ("type" . "string"))))
  ) ;json-schema-valid?
) ;check-true
(check-false
  (json-schema-valid? '(("alpha" . 1))
    '(("patternProperties"
       ("^a" ("type" . "integer"))
       ("pha$" ("type" . "string"))))
  ) ;json-schema-valid?
) ;check-false

;; 匹配到的属性不算 additionalProperties。
(check-true
  (json-schema-valid? '(("alpha" . 1))
    '(("patternProperties" ("^a" ("type" . "integer")))
      ("additionalProperties" . #f))
  ) ;json-schema-valid?
) ;check-true
(check-false
  (json-schema-valid? '(("beta" . 1))
    '(("patternProperties" ("^a" ("type" . "integer")))
      ("additionalProperties" . #f))
  ) ;json-schema-valid?
) ;check-false

(check-report)
