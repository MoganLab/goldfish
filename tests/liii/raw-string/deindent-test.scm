(import (liii check)
        (liii raw-string)
) ;import

(check-set-mode! 'report-failed)

;; deindent
;; 移除原始字符串中用于代码缩进的空格。
;;
;; 语法
;; ----
;; (deindent #"" ... "")
;; (deindent #"TAG" ... "TAG")
;;
;; 参数
;; ----
;; 原始字符串字面量，使用 #"" ... "" 或带标签的 #"TAG" ... "TAG" 格式。
;;
;; 返回值
;; ----
;; string
;;   移除缩进后的字符串。
;;
;; 描述
;; ----
;; deindent 是一个宏，用于处理多行原始字符串，自动移除用于代码格式化的缩进空格。
;; 缩进的计算基于结束分隔符所在行的缩进级别，所有内容行会相应去除该级别的缩进。
;;
;; 规则：
;; - 原始字符串必须从换行符开始（ opening delimiter 后紧跟换行）
;; - 结束分隔符必须单独占一行
;; - 所有内容行必须至少包含结束行同等数量的缩进空格
;; - 不支持制表符缩进

;; 基本多行文本处理
(check
 (&- #""
  多行
  保留与最后一样一致的换行和缩进（空格）
  使用 deindent/&- 来对齐缩进
  ""
 ) ;&-
 => "多行\n保留与最后一样一致的换行和缩进（空格）\n使用 deindent/&- 来对齐缩进"
) ;check

;; 空字符串
(check (&- #""
  "") => "")

(check (&- #""

  "") => "")

;; 错误：原始字符串必须在 opening delimiter 后换行开始
(check-catch 'value-error (&- #"" ""))
(check-catch 'value-error (&- #"" hello ""))

;; 基本多行处理
(check
 (&- #""
  第一行
  第二行
  第三行
  ""
 ) ;&-
 => "第一行\n第二行\n第三行"
) ;check

;; 不同缩进级别
(check
 (&- #""
  第一行
    第二行（缩进2格）
      第三行（缩进4格）
  第四行
  ""
 ) ;&-
 => "第一行\n  第二行（缩进2格）\n    第三行（缩进4格）\n第四行"
) ;check

;; 嵌套缩进结构
(check
 (&- #""
  外层
    内层1
      更内层
    内层2
  外层结束
  ""
 ) ;&-
 => "外层\n  内层1\n    更内层\n  内层2\n外层结束"
) ;check

;; 空行保留
(check
 (&- #""
  第一行

  第三行（上下有空行）

  第五行（上有空行）
  ""
 ) ;&-
 => "第一行\n\n第三行（上下有空行）\n\n第五行（上有空行）"
) ;check

;; 不支持制表符：行2不以原始字符串结束行相同的空白字符开头
(check-catch 'value-error
 (&- #""
  第一行
	第二行（使用制表符）
	  第三行（混合缩进）
  "")
) ;check-catch

;; 特殊字符
(check
 (&- #""
  "引号内的文本"
  '单引号'
  `反引号`
  特殊字符：!@#$%^&*()_+-={}[]|\:;"'<>,.?/
  ""
 ) ;&-
 => "\"引号内的文本\"\n'单引号'\n`反引号`\n特殊字符：!@#$%^&*()_+-={}[]|\\:;\"'<>,.?/"
) ;check

;; Unicode 字符
(check
 (&- #""
  Hello 世界
  🌍🌎🌏
  Emoji测试 🚀🎉
  中文、English、にほんご
  ""
 ) ;&-
 => "Hello 世界\n🌍🌎🌏\nEmoji测试 🚀🎉\n中文、English、にほんご"
) ;check

;; Scheme 代码块
(check
 (&- #""
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
 => "(define (factorial n)\n  (if (<= n 1)\n      1\n      (* n (factorial (- n 1)))))\n\n(define (fibonacci n)\n  (cond\n    ((= n 0) 0)\n    ((= n 1) 1)\n    (else (+ (fibonacci (- n 1))\n             (fibonacci (- n 2))))))"
) ;check

;; HTML 内容
(check
 (&- #"HTML"
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
 => "<div class=\"container\">\n  <h1>标题</h1>\n  <p>段落内容</p>\n  <ul>\n    <li>项目1</li>\n    <li>项目2</li>\n  </ul>\n</div>"
) ;check

;; SQL 查询
(check
 (&- #""
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
 => "SELECT\n  users.id,\n  users.name,\n  COUNT(orders.id) as order_count\nFROM users\nLEFT JOIN orders ON users.id = orders.user_id\nWHERE users.active = TRUE\nGROUP BY users.id, users.name\nORDER BY order_count DESC"
) ;check

;; JSON 内容
(check
 (&- #""
  {
    "name": "测试",
    "version": "1.0.0",
    "dependencies": {
      "library1": "^1.2.3",
      "library2": "~2.0.0"
    },
    "scripts": {
      "start": "node index.js",
      "test": "jest"
    }
  }
  ""
 ) ;&-
 => "{\n  \"name\": \"测试\",\n  \"version\": \"1.0.0\",\n  \"dependencies\": {\n    \"library1\": \"^1.2.3\",\n    \"library2\": \"~2.0.0\"\n  },\n  \"scripts\": {\n    \"start\": \"node index.js\",\n    \"test\": \"jest\"\n  }\n}"
) ;check

;; 复杂缩进变化
(check
 (&- #""
  第一行（无缩进）
    第二行（缩进2格）
     第三行（缩进3格）
    第四行（又回到2格）
  第五行（回到无缩进）
          第六行（缩进8格，移除 closing line 的缩进）
  ""
 ) ;&-
 => "第一行（无缩进）\n  第二行（缩进2格）\n   第三行（缩进3格）\n  第四行（又回到2格）\n第五行（回到无缩进）\n        第六行（缩进8格，移除 closing line 的缩进）"
) ;check

;; 前导空格处理
(check
 (&- #""
    这行前面有0个空格
    这行前面有0个空格
      这行前面有2个空格
  ""
 ) ;&-
 => "  这行前面有0个空格\n  这行前面有0个空格\n    这行前面有2个空格"
) ;check

;; 最小缩进对齐
(check
 (&- #""
  所有行应该对齐到最小缩进级别：
    这一行缩进2格
      这一行缩进4格
    这一行又回到2格
  这一行没有缩进
  ""
 ) ;&-
 => "所有行应该对齐到最小缩进级别：\n  这一行缩进2格\n    这一行缩进4格\n  这一行又回到2格\n这一行没有缩进"
) ;check

;; 空行在开头和结尾
(check
 (&- #""

  只有这一行

  ""
 ) ;&-
 => "\n只有这一行\n"
) ;check

;; 长文本段落
(check
 (&- #""
  这是一个较长的文本段落，
  用来测试&-宏处理多行长文本的能力。
  文本可以包含各种标点符号，
  如逗号、句号、问号？感叹号！
  也可以包含数字：1234567890
  以及各种括号：()[]{}<>
  测试结束。
  ""
 ) ;&-
 => "这是一个较长的文本段落，\n用来测试&-宏处理多行长文本的能力。\n文本可以包含各种标点符号，\n如逗号、句号、问号？感叹号！\n也可以包含数字：1234567890\n以及各种括号：()[]{}<>\n测试结束。"
) ;check

;; 多行连续内容
(check
 (&- #""
  行1
  行2
  行3
  行4
  行5
  行6
  行7
  行8
  行9
  行10
  ""
 ) ;&-
 => "行1\n行2\n行3\n行4\n行5\n行6\n行7\n行8\n行9\n行10"
) ;check

(check-report)
