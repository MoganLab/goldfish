;; (liii json schema error) 模块介绍与用法示例
;;
;; (liii json schema error) 定义 JSON Schema 校验错误对象 <json-schema-error>，
;; 以及创建错误与读取错误各字段的访问器。

(import (liii json schema) (liii json schema error) (liii check))

;; ==== 模块概述 ====
;;
;; - json-schema-error?      - 判断是否为错误对象
;; - make-validation-error   - 显式构造错误记录
;; - validation-error        - 构造类别为 validation 的错误
;; - json-schema-error-*     - 读取 kind/message/keyword/instance 等字段
;; - value->display          - 将任意值转为 write 风格的显示字符串
;;
;; 错误对象由校验入口产生，也可手工构造用于测试或自定义报告。

;; ==== 如何查看函数的文档和用例 ====
;;
;; 查看模块文档：
;;   gf doc liii/json/schema/error
;;
;; 查看具体函数文档：
;;   gf doc "json-schema-error-kind"
;;   gf doc "make-validation-error"

;; ==== 函数分类索引 ====
;;
;; 一、谓词与构造器
;;   json-schema-error?   - 判断是否为错误对象
;;   make-validation-error - 构造错误记录（10 个字段）
;;   validation-error     - 构造类别为 validation 的错误
;;   value->display       - 值转 write 风格字符串
;;
;; 二、字段访问器
;;   json-schema-error-kind      - 错误类别（validation / schema）
;;   json-schema-error-message   - 人类可读消息
;;   json-schema-error-keyword   - 触发错误的关键字
;;   json-schema-error-keyword-value - 关键字的取值
;;   json-schema-error-instance  - 触发位置的实例
;;   json-schema-error-schema    - 触发位置的 Schema
;;   json-schema-error-path      - 实例路径（倒序段列表）
;;   json-schema-error-schema-path - Schema 路径（倒序段列表）
;;   json-schema-error-context   - 组合关键字子错误
;;   json-schema-error-cause     - 底层原因
;;   json-schema-error-absolute-path - 从根开始的完整实例路径
;;   json-schema-error-absolute-schema-path - 从根开始的完整 Schema 路径
;;   json-schema-error-json-path - JSONPath 风格显示路径

;; ==== 常见用法示例 ====

;; 示例1：从校验入口取得错误对象并读取字段

(define err (car (json-schema-errors "x" '(("type" . "integer")))))

(json-schema-error? err)
(json-schema-error-kind err)
(json-schema-error-keyword err)
(json-schema-error-instance err)
(json-schema-error-json-path err)

;; 示例2：组合关键字失败时读取子错误上下文

(let* ((errors (json-schema-errors 1 '(("anyOf"
                                        . #((("type" . "string"))
                                            (("minimum" . 2))
                                          ) ;#
                                       )))
       ) ;errors
       (parent (car errors))
       (children (json-schema-error-context parent))
      ) ;
  (map json-schema-error-keyword children)
) ;let*

;; 示例3：手工构造错误记录

(define custom (validation-error "msg" "type" "string" 1 #t '() '() '() #f))

(json-schema-error-kind custom)
(json-schema-error-cause custom)

(check-report)
