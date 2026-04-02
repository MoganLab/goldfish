;; (liii check) 模块函数分类索引
;;
;; check 是 Goldfish 当前项目中使用的轻量测试库封装。
;; 它基于 SRFI-78，提供基础断言、异常断言、结果汇总，以及浮点近似断言。

;; ==== 常见用法示例 ====
(import (liii check))

;; 示例1：普通断言
;; (check (+ 1 2) => 3)

;; 示例2：布尔断言
;; (check-true (number? 42))

;; 示例3：浮点近似断言
;; (check-approx (+ 0.1 0.2) => 0.3 :rel-tol 1e-12 :abs-tol 1e-12)

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/check "check"
;;   bin/gf doc liii/check "check-approx"

;; ==== 函数分类索引 ====

;; 一、基础断言
;;   test             - `check` 的简写别名
;;   check            - 精确结果断言
;;   check-true       - 断言表达式结果为 #t
;;   check-false      - 断言表达式结果为 #f
;;   check-approx     - 断言数值结果在给定容差内近似相等
;;   check-float      - 旧的浮点辅助比较函数

;; 二、异常与汇总
;;   check-catch      - 断言表达式抛出指定错误
;;   check-set-mode!  - 设置测试输出模式
;;   check-report     - 输出测试汇总并在失败时退出
;;   check-failed?    - 判断当前是否存在失败用例

;; 三、底层接口
;;   check:proc       - SRFI-78 底层断言过程
