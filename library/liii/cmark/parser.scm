
(define-library (liii cmark parser)
  ;; 生产级门面：只 re-export 生产代码消费的 3 个解析入口
  ;; （块级解析的围栏/缩进代码块、行内解析），其余解析函数
  ;; 由测试直接 import 子模块（block-parser / inline-parser）。
  (import (scheme base))
  (import (liii cmark block-parser))
  (import (liii cmark inline-parser))
  (export parse-inlines)
  (export parse-fenced-code-block)
  (export parse-indented-code-block)
) ;define-library
