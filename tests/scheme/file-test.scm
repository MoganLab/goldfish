;; # (scheme file) 库测试和文档
;;
;; (scheme file) 是 R7RS 标准库，提供文件操作相关的函数。
;;
;; ## 导出的函数
;;
;; | 函数名 | 说明 | 测试文件 |
;; |--------|------|----------|
;; | call-with-input-file | 以输入端口打开文件并调用过程 | call-with-input-file-test.scm |
;; | call-with-output-file | 以输出端口打开文件并调用过程 | call-with-output-file-test.scm |
;; | delete-file | 删除文件 | delete-file-test.scm |
;; | file-exists? | 检查文件是否存在 | file-exists-p-test.scm |
;; | open-binary-input-file | 以二进制输入模式打开文件 | open-binary-input-file-test.scm |
;; | open-binary-output-file | 以二进制输出模式打开文件 | open-binary-output-file-test.scm |
;; | open-input-file | 以输入模式打开文件 | open-input-file-test.scm |
;; | open-output-file | 以输出模式打开文件 | open-output-file-test.scm |
;; | with-input-from-file | 将当前输入端口重定向到文件 | with-input-from-file-test.scm |
;; | with-output-to-file | 将当前输出端口重定向到文件 | with-output-to-file-test.scm |
;;
;; ## 测试说明
;;
;; 每个函数都有独立的测试文件，位于 tests/scheme/file/ 目录下。
;; 测试内容包括：
;; - 基本功能测试
;; - 中文文件名测试
;; - 错误处理测试（参数类型错误等）