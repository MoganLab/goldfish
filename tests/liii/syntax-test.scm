;; (liii syntax) 模块函数分类索引
;;
;; `(liii syntax)` 提供 S 表达式语法形式判定与卫生宏底层句法闭包操作。


;; ==== 使用说明 ====
;;   (import (liii syntax))


;; ==== 函数分类索引 ====


;; 一、语法形式检查
;;   quote-form?                 - 检查是否为 quote 形式：(quote x) 或 (#_quote x)
;;   unquote-form?               - 检查是否为 unquote 形式：(unquote x) 或 (unquote-splicing x)

;; 二、句法闭包（syntactic closure）
;;   make-syntactic-closure      - 创建句法闭包：(make-syntactic-closure env free-vars expr)
;;   syntactic-closure?          - 判断是否为句法闭包
;;   syntactic-closure-env       - 取出闭包捕获的词法环境
;;   syntactic-closure-expr      - 取出闭包包装的表达式
;;   syntactic-closure-free-vars - 取出闭包的自由变量列表
;;   syntactic-closure-rename    - 取出闭包的重命名过程
;;   syntactic-closure-set-rename! - 设置闭包的重命名过程（破坏性）

;; 三、标识符操作
;;   identifier?                 - 判断是否为标识符（符号或包装符号的句法闭包）
;;   identifier->symbol          - 将标识符还原为底层符号
;;   identifier=?                - 判定两标识符在各自环境中是否指向同一绑定
;;   strip-syntactic-closures    - 递归剥除表达式中的所有句法闭包包装
;;   resolve-syntactic-closures  - 将宏展开结果消解为干净的原生 S 表达式
