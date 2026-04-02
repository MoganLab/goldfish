;; (liii check) 模块函数分类索引
;;
;; check 是 Goldfish 当前项目中使用的轻量测试库封装。
;; 它基于 SRFI-78，提供基础断言、异常断言、结果汇总，以及浮点近似断言。

;; ==== 常见用法示例 ====
;;
;; 示例1：`summary` 模式
;; 适合 CI 或批量运行测试时只看汇总结果。
;;
;; (import (liii check))
;; (check-set-mode! 'summary)
;; (check (+ 1 2) => 3)
;; (check-true (number? 42))
;; (check-approx (+ 0.1 0.2) => 0.3 :rel-tol 1e-12 :abs-tol 1e-12)
;; (check-report)
;;
;; 示例2：`report-failed` 模式
;; 适合日常开发时只输出失败断言，减少噪声。
;;
;; (import (liii check))
;; (check-set-mode! 'report-failed)
;; (check (string-length "goldfish") => 8)
;; (check-false (null? '(a b)))
;; (check-catch 'wrong-type-arg (car 123))
;; (check-report)
;;
;; 示例3：`report` 模式
;; 适合调试时查看每条断言的执行结果。
;;
;; (import (liii check))
;; (check-set-mode! 'report)
;; (check '(a b c) => '(a b c))
;; (test (boolean? #t) #t)
;; (check-false (zero? 1))
;; (check-report)

;; ==== 使用场景 ====
;;
;; 1. 为纯函数编写精确断言测试
;;    适合对字符串处理、列表处理、路径处理等函数检查确定结果。
;;
;; 2. 为谓词和条件分支编写布尔断言测试
;;    适合验证某个表达式是否返回 #t 或 #f，而不需要手动写出完整比较式。
;;
;; 3. 为异常路径编写错误断言测试
;;    适合检查非法参数、缺少参数、类型错误等场景是否抛出预期错误。
;;
;; 4. 为数值计算和浮点结果编写近似断言测试
;;    适合三角函数、统计计算、数值迭代等存在浮点误差的结果校验。
;;
;; 5. 为一组测试统一输出汇总结果
;;    适合在文件末尾调用 `check-report`，集中查看通过数和失败数。
;;
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

;; 二、异常与汇总
;;   check-catch      - 断言表达式抛出指定错误
;;   check-set-mode!  - 设置测试输出模式
;;   check-report     - 输出测试汇总并在失败时退出
;;   check-failed?    - 判断当前是否存在失败用例
