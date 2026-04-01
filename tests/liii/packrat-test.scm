;; (liii packrat) 模块函数分类索引
;;
;; (liii packrat) 提供 Packrat Parsing 实现，支持 memoization 的递归下降解析。
;; 该库基于解析表达式文法 (PEG)，适合构建高性能的解析器。
;; 与 (liii parser) 相比，packrat 解析器通过记忆化技术避免左递归和重复解析问题。

;; ==== 常见用法示例 ====
(import (liii packrat))

;; 示例1：创建一个简单的数值解析器
(define simple-parser
  (packrat-parser expr
    (expr ((a <- 'num) a)
          ((a <- 'id) a)
    ) ;expr
  ) ;packrat-parser
) ;define

;; 示例2：使用 token 生成器驱动解析
(define (generator tokens)
  (let ((stream tokens))
    (lambda ()
      (if (null? stream)
          (values #f #f)
          (let ((token (car stream)))
            (set! stream (cdr stream))
            (values #f token)
          ) ;let
      ) ;if
    ) ;lambda
  ) ;let
) ;define

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/packrat "function-name"

;; ==== 函数分类索引 ====
;;
;; 一、parse-result 解析结果操作
;;   make-result                  - 构造成功解析结果
;;   parse-result?                - 判断是否为解析结果对象
;;   parse-result-successful?     - 判断解析是否成功
;;   parse-result-semantic-value  - 获取语义值
;;   parse-result-next            - 获取后续解析位置
;;   make-expected-result         - 构造期望失败结果
;;   make-message-result          - 构造带消息的失败结果
;;
;; 二、parse-position 解析位置操作
;;   make-parse-position          - 构造解析位置
;;   parse-position?              - 判断是否为位置对象
;;   parse-position-file          - 获取文件名
;;   parse-position-line          - 获取行号
;;   parse-position-column        - 获取列号
;;
;; 三、parse-results 解析结果流操作
;;   base-generator->results      - 将生成器转为解析结果流
;;   parse-results?               - 判断是否为解析结果流
;;   parse-results-token-kind     - 获取 token 类型
;;   parse-results-token-value    - 获取 token 值
;;
;; 四、parse-error 解析错误操作
;;   make-error-expected          - 构造期望错误
;;   make-error-message           - 构造消息错误
;;   parse-error?                 - 判断是否为错误对象
;;   parse-error-position         - 获取错误位置
;;
;; 五、combinator 组合子
;;   packrat-check-base           - 匹配指定类型 token
;;   packrat-or                   - 多选组合子
;;   packrat-check                - 结果变换组合子
;;   packrat-unless               - 排除规则组合子
;;
;; 六、解析器构造
;;   packrat-parser               - 创建完整解析器
