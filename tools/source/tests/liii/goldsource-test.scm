;; (liii goldsource) 模块函数分类索引
;;
;; goldsource 是 `gf source` 子命令在 Scheme 层的实现入口。
;; 它负责解析库查询、定位源文件，并将源代码直接输出到标准输出。

;; ==== 常见用法示例 ====
;; 添加 tools/goldsource 到 load path，以便导入 (liii goldsource)
(set! *load-path*
  (cons "tools/goldsource" *load-path*)
) ;set!

(import (liii goldsource))

;; 示例1：解析 gf source 命令行参数
(parse-source-args '("bin/gf" "source" "liii/string")
) ;parse-source-args

;; 示例2：定位可见库的源文件
(source-library-path "liii/string")

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/goldsource "parse-source-args"
;;   bin/gf doc liii/goldsource "source-library-path"

;; ==== 函数分类索引 ====

;; 一、参数解析
;;   parse-source-args      - 解析 `gf source` 命令参数
;;   library-query?         - 判断查询是否为库级查询
;;   parse-library-query    - 解析库查询字符串

;; 二、路径定位
;;   find-visible-library-root - 查找当前 *load-path* 中可见的库根目录
;;   source-library-path    - 计算库源码文件路径

;; 三、CLI 入口
;;   run-goldsource         - 执行 `gf source` 主流程
;;   main                   - 命令行入口
