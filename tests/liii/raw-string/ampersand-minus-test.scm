(import (liii check) (liii raw-string))

(check-set-mode! 'report-failed)

;; &-
;; 对多行 raw string 字面量执行去缩进。
;;
;; 语法
;; ----
;; (&- raw-string-literal)
;;
;; 参数
;; ----
;; raw-string-literal : string
;; 由 `(liii raw-string)` reader 读取的多行 raw string 字面量。
;;
;; 返回值
;; ----
;; string
;; 去掉公共缩进后的多行字符串。
;;
;; 说明
;; ----
;; `&-` 是 `deindent` 的别名语法，更适合直接包裹多行 raw string。
;; 它常用于嵌入 HTML、SQL、JSON 和代码片段。
;;
;; 错误处理
;; ----
;; value-error
;; raw string 不是从 opening delimiter 后的新行开始，或缩进不满足规则时抛出。

(check (&- #""
  多行
  保留与最后一样一致的换行和缩进（空格）
  使用 deindent/&- 来对齐缩进
  ""
       ) ;&-
  =>
  "多行\n保留与最后一样一致的换行和缩进（空格）\n使用 deindent/&- 来对齐缩进"
) ;check
(check (&- #""
  ""
       ) ;&-
  =>
  ""
) ;check
(check (&- #""

  ""
       ) ;&-
  =>
  ""
) ;check
(check (&- #""
  "引号内的文本"
  '单引号'
  `反引号`
  特殊字符：!@#$%^&*()_+-={}[]|\:;"'<>,.?/
  ""
       ) ;&-
  =>
  "\"引号内的文本\"\n'单引号'\n`反引号`\n特殊字符：!@#$%^&*()_+-={}[]|\\:;\"'<>,.?/"
) ;check
(check (&- #""
  Hello 世界
  🌍🌎🌏
  Emoji测试 🚀🎉
  中文、English、にほんご
  ""
       ) ;&-
  =>
  "Hello 世界\n🌍🌎🌏\nEmoji测试 🚀🎉\n中文、English、にほんご"
) ;check
(check (&- #""
  (define (factorial n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))

  (define (fibonacci n)
    (cond
      ((= n 0) 0)
      ((= n 1) 1)
      (else (+ (fibonacci (- n 1))
               (fibonacci (- n 2))))))
  ""
       ) ;&-
  =>
  "(define (factorial n)\n  (if (<= n 1)\n      1\n      (* n (factorial (- n 1)))))\n\n(define (fibonacci n)\n  (cond\n    ((= n 0) 0)\n    ((= n 1) 1)\n    (else (+ (fibonacci (- n 1))\n             (fibonacci (- n 2))))))"
) ;check
(check (&- #"HTML"
  <div class="container">
    <h1>标题</h1>
    <p>段落内容</p>
    <ul>
      <li>项目1</li>
      <li>项目2</li>
    </ul>
  </div>
  "HTML"
       ) ;&-
  =>
  "<div class=\"container\">\n  <h1>标题</h1>\n  <p>段落内容</p>\n  <ul>\n    <li>项目1</li>\n    <li>项目2</li>\n  </ul>\n</div>"
) ;check
(check (&- #""
  SELECT
    users.id,
    users.name,
    COUNT(orders.id) as order_count
  FROM users
  LEFT JOIN orders ON users.id = orders.user_id
  WHERE users.active = TRUE
  GROUP BY users.id, users.name
  ORDER BY order_count DESC
  ""
       ) ;&-
  =>
  "SELECT\n  users.id,\n  users.name,\n  COUNT(orders.id) as order_count\nFROM users\nLEFT JOIN orders ON users.id = orders.user_id\nWHERE users.active = TRUE\nGROUP BY users.id, users.name\nORDER BY order_count DESC"
) ;check
(check-catch 'value-error (&- #"" ""))
(check-catch 'value-error
  (&- #"" hello "")
) ;check-catch
(check-catch 'value-error
  (&- #""
  第一行
	第二行（使用制表符）
	  第三行（混合缩进）
  ""
  ) ;&-
) ;check-catch
(check-report)
