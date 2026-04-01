;; (liii golddoc) 模块函数分类索引
;;
;; golddoc 是 `gf doc` 子命令在 Scheme 层的实现入口。
;; 它负责解析查询、定位文档文件、加载索引，并提供模糊匹配建议。

;; ==== 常见用法示例 ====
;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
(set! *load-path* (cons "tools/golddoc" *load-path*))

(import (liii golddoc))

;; 示例1：解析 gf doc 命令行参数
(parse-doc-args '("bin/gf" "doc" "liii/string")) ; => '(library "liii/string")

;; 示例2：把导出名映射成测试文件 stem
(exported-name->test-stem "njson-set!") ; => "njson-set-bang"

;; 示例3：计算模糊匹配编辑距离
(bounded-levenshtein-distance "string-spilt" "string-split") ; => 2

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/golddoc "parse-doc-args"
;;   bin/gf doc liii/golddoc "suggest-candidates"

;; ==== 函数分类索引 ====

;; 一、参数解析
;; 用于识别 `gf doc` 输入参数的函数
;;   parse-doc-args               - 解析命令行参数
;;   library-query?               - 判断查询是否为库级查询
;;   parse-library-query          - 解析库查询字符串
;;   excluded-test-group?         - 判断测试分组是否应被排除

;; 二、路径与文档定位
;; 用于查找库根目录、测试目录和文档路径的函数
;;   find-visible-library-root    - 查找可见库根目录
;;   find-tests-root-for-load-root - 根据 load root 查找 tests 根目录
;;   library-doc-path             - 计算库文档路径
;;   exported-name->test-stem     - 导出名转测试 stem
;;   library-documented-functions - 获取库中有文档的函数列表
;;   function-doc-path            - 计算函数文档路径

;; 三、索引读取与构建
;; 用于处理函数索引文件的函数
;;   index-entry->library-query   - 索引项转库查询
;;   find-function-index-paths    - 查找函数索引路径
;;   load-function-index          - 读取函数索引
;;   visible-function-names       - 获取当前可见函数名
;;   visible-libraries-for-function - 查找某个函数在哪些库可见
;;   build-function-indexes!      - 构建函数索引文件

;; 四、模糊匹配与建议
;; 用于文档搜索建议和模糊匹配的函数
;;   max-fuzzy-edit-distance      - 获取允许的最大编辑距离
;;   bounded-levenshtein-distance - 计算有界编辑距离
;;   suggest-candidates           - 给出候选建议
;;   suggest-library-functions    - 对库内函数给出建议
;;   suggest-visible-functions    - 对当前可见函数给出建议

;; 五、CLI 入口
;; 用于驱动 `gf doc` 命令的函数
;;   run-golddoc                  - 执行 golddoc 主流程
;;   main                         - 命令行入口
