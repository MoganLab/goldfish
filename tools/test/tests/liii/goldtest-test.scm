;; (liii goldtest) 模块函数分类索引
;;
;; goldtest 是 `gf test` 子命令在 Scheme 层的实现入口。
;; 它负责解析测试参数、筛选目标文件、执行测试并汇总结果。

;; ==== 常见用法示例 ====
;; 添加 tools/goldtest 到 load path，以便导入 (liii goldtest)
(set! *load-path* (cons "tools/goldtest" *load-path*))

(import (liii goldtest))

;; 示例1：解析 gf test 命令行参数
(parse-test-args '("bin/gf" "test" "json")) ; => '(pattern . "json")

;; 示例2：按模式过滤测试文件
(filter-test-files '("tests/liii/json-test.scm" "tests/liii/list-test.scm") 'pattern "json")
;; => '("tests/liii/json-test.scm")

;; 示例3：查找整个 tests 目录中的测试文件
(find-test-files "tests/liii")

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/goldtest "parse-test-args"
;;   bin/gf doc liii/goldtest "find-test-files"

;; ==== 函数分类索引 ====

;; 一、参数解析
;; 用于识别 `gf test` 输入参数的函数
;;   parse-test-args     - 解析测试命令参数

;; 二、测试发现与过滤
;; 用于查找和筛选测试文件的函数
;;   filter-test-files   - 按模式过滤测试文件列表
;;   find-test-files     - 递归查找测试文件

;; 三、执行与入口
;; 用于驱动 `gf test` 命令的函数
;;   run-goldtest        - 执行测试主流程
;;   main                - 命令行入口
