;; (liii base) 模块函数分类索引
;;
;; `(liii base)` 是 `(scheme base)` 的扩展库，包含了 R7RS `scheme base` 的所有功能，
;; 并额外提供了来自 SRFI-2、SRFI-8 的实用函数，以及若干扩展工具。
;;
;; ==== 使用说明 ====
;; 如需使用 R7RS 标准函数，请查看：
;;   bin/gf doc scheme/base
;;
;; 本库扩展函数列表如下：


;; ==== 扩展函数分类索引 ====


;; 一、SRFI-2 条件绑定
;;   and-let*             - 带条件的 let* 绑定


;; 二、SRFI-8 多值处理
;;   receive              - 接收多个返回值


;; 三、S7 扩展
;;   define*              - 带默认参数和可选参数的 define
;;   procedure-source     - 获取过程的源代码定义
;;   procedure-arglist    - 获取过程的参数列表
;;   arity                - 获取过程可接受的参数数量范围
;;   defined?             - 检查符号是否已定义
;;   object->string      - 将对象转换为字符串表示
;;   eval-string          - 将字符串作为 Scheme 代码求值
;;   signature            - 获取函数的类型签名
;;   keyword?             - 判断是否为关键字
;;   string->keyword      - 字符串转关键字
;;   symbol->keyword      - 符号转关键字
;;   keyword->symbol      - 关键字转符号


;; 四、实用工具函数
;;   square               - 计算平方
;;   loose-car            - 宽松地获取 car（空列表返回空列表）
;;   loose-cdr            - 宽松地获取 cdr（空列表返回空列表）
;;   compose              - 组合多个函数
;;   identity             - 原样返回输入值
;;   any?                 - 恒返回 #t 的谓词函数


;; 五、类型检查语法
;;   typed-lambda         - 带类型标注的 lambda 语法
