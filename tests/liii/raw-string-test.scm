;
; Copyright (C) 2026 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii check)
        (liii raw-string))

(check-set-mode! 'report-failed)

(check #""""               => "")
(check #"" ""              => " ")
(check #""a""              => "a")
(check #""\""              => "\\")
(check #"-"""-"            => "\"")
(check #"-"\""-"           => "\\\"")
(check #"-"#"()""-"        => "#\"()\"")
(check #"-"#""a"""-"       =>  "#\"\"a\"\"")
(check #"-"ends with \""-" => "ends with \\\"")

(check
  #""multiline
string""
  => "multiline\nstring")

(check
  #""
    no whitespace stripping""
  => "\n    no whitespace stripping")

(check
  #""
    no whitespace stripping
  ""
  => "\n    no whitespace stripping\n  ")

(check
  #""
  注释 ;; comment
  ""
  => "\n  注释 ;; comment\n  ")

(check
  #"HTML"
<!DOCTYPE html>
<html>
  <head><title>"测试页面"</title></head>
  <body>
    <p>这里有很多"引号"</p>
  </body>
</html>
  "HTML"
  => "\n<!DOCTYPE html>\n<html>\n  <head><title>\"测试页面\"</title></head>\n  <body>\n    <p>这里有很多\"引号\"</p>\n  </body>\n</html>\n  ")

(check
  #"HTML"<!DOCTYPE html>
<html>
  <head><title>"测试页面"</title></head>
  <body>
    <p>这里有很多"引号"</p>
  </body>
</html>
  "HTML"
  => "<!DOCTYPE html>\n<html>\n  <head><title>\"测试页面\"</title></head>\n  <body>\n    <p>这里有很多\"引号\"</p>\n  </body>\n</html>\n  ")

(check
  #"HTML"<!DOCTYPE html>
<html>
  <head><title>"测试页面"</title></head>
  <body>
    <p>这里有很多"引号"</p>
  </body>
</html>"HTML"
  => "<!DOCTYPE html>\n<html>\n  <head><title>\"测试页面\"</title></head>\n  <body>\n    <p>这里有很多\"引号\"</p>\n  </body>\n</html>")

;; ====================
;; deindent / &-
;; ====================

; Basic functionality tests for &- macro
(check
 (&- #""
  多行
  保留与最后一样一致的换行和缩进（空格）
  使用 deindent/&- 来对齐缩进
  "")
 => "多行\n保留与最后一样一致的换行和缩进（空格）\n使用 deindent/&- 来对齐缩进")

; Test with empty string
(check (&- #""
  "") => "")

(check (&- #""

  "") => "")

; error: Raw string must start on a new line after the opening delimiter
(check-catch 'value-error (&- #"" ""))
(check-catch 'value-error (&- #"" hello ""))

(check
 (&- #""
  第一行
  第二行
  第三行
  "")
 => "第一行\n第二行\n第三行")

(check
 (&- #""
  第一行
    第二行（缩进2格）
      第三行（缩进4格）
  第四行
  "")
 => "第一行\n  第二行（缩进2格）\n    第三行（缩进4格）\n第四行")

(check
 (&- #""
  外层
    内层1
      更内层
    内层2
  外层结束
  "")
 => "外层\n  内层1\n    更内层\n  内层2\n外层结束")

(check
 (&- #""
  第一行

  第三行（上下有空行）

  第五行（上有空行）
  "")
 => "第一行\n\n第三行（上下有空行）\n\n第五行（上有空行）")

; does not support tab: Line 2 does not start with the same whitespace as the closing line of the raw string
(check-catch 'value-error
 (&- #""
  第一行
	第二行（使用制表符）
	  第三行（混合缩进）
  ""))

; Test with special characters
(check
 (&- #""
  "引号内的文本"
  '单引号'
  `反引号`
  特殊字符：!@#$%^&*()_+-={}[]|\:;"'<>,.?/
  "")
 => "\"引号内的文本\"\n'单引号'\n`反引号`\n特殊字符：!@#$%^&*()_+-={}[]|\\:;\"'<>,.?/")

; Test with Unicode characters
(check
 (&- #""
  Hello 世界
  🌍🌎🌏
  Emoji测试 🚀🎉
  中文、English、にほんご
  "")
 => "Hello 世界\n🌍🌎🌏\nEmoji测试 🚀🎉\n中文、English、にほんご")

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
  "")
 => "(define (factorial n)\n  (if (<= n 1)\n      1\n      (* n (factorial (- n 1)))))\n\n(define (fibonacci n)\n  (cond\n    ((= n 0) 0)\n    ((= n 1) 1)\n    (else (+ (fibonacci (- n 1))\n             (fibonacci (- n 2))))))")

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
  "HTML")
 => "<div class=\"container\">\n  <h1>标题</h1>\n  <p>段落内容</p>\n  <ul>\n    <li>项目1</li>\n    <li>项目2</li>\n  </ul>\n</div>")

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
  "")
 => "SELECT \n  users.id,\n  users.name,\n  COUNT(orders.id) as order_count\nFROM users\nLEFT JOIN orders ON users.id = orders.user_id\nWHERE users.active = TRUE\nGROUP BY users.id, users.name\nORDER BY order_count DESC")

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
  "")
 => "{\n  \"name\": \"测试\",\n  \"version\": \"1.0.0\",\n  \"dependencies\": {\n    \"library1\": \"^1.2.3\",\n    \"library2\": \"~2.0.0\"\n  },\n  \"scripts\": {\n    \"start\": \"node index.js\",\n    \"test\": \"jest\"\n  }\n}")

(check
 (&- #""
  第一行（无缩进）
    第二行（缩进2格）
     第三行（缩进3格）
    第四行（又回到2格）
  第五行（回到无缩进）
          第六行（缩进8格，移除 closing line 的缩进）
  "") ; closing line
 => "第一行（无缩进）\n  第二行（缩进2格）\n   第三行（缩进3格）\n  第四行（又回到2格）\n第五行（回到无缩进）\n        第六行（缩进8格，移除 closing line 的缩进）")

; Test with trailing spaces in lines
(check
 (&- #""
  行尾有空格   
  行尾有多个空格     
  正常行
  "")
 => "行尾有空格   \n行尾有多个空格     \n正常行")

(check
 (&- #""
    这行前面有0个空格后面也有4个空格    
    这行前面有0个空格
      这行前面有2个空格
  "")
 => "  这行前面有0个空格后面也有4个空格    \n  这行前面有0个空格\n    这行前面有2个空格")

(check
 (&- #""
  所有行应该对齐到最小缩进级别：
    这一行缩进2格
      这一行缩进4格
    这一行又回到2格
  这一行没有缩进
  "")
 => "所有行应该对齐到最小缩进级别：\n  这一行缩进2格\n    这一行缩进4格\n  这一行又回到2格\n这一行没有缩进")

(check
 (&- #""

  只有这一行

  "")
 => "\n只有这一行\n")

(check
 (&- #""
  这是一个较长的文本段落，
  用来测试&-宏处理多行长文本的能力。
  文本可以包含各种标点符号，
  如逗号、句号、问号？感叹号！
  也可以包含数字：1234567890
  以及各种括号：()[]{}<>
  测试结束。
  "")
 => "这是一个较长的文本段落，\n用来测试&-宏处理多行长文本的能力。\n文本可以包含各种标点符号，\n如逗号、句号、问号？感叹号！\n也可以包含数字：1234567890\n以及各种括号：()[]{}<>\n测试结束。")

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
  "")
 => "行1\n行2\n行3\n行4\n行5\n行6\n行7\n行8\n行9\n行10")

(check-report)
