(import (liii check)
        (liii raw-string)
) ;import

(check-set-mode! 'report-failed)

;; #"..."
;; 读取 SRFI-267 风格的 raw string 字面量。
;;
;; 语法
;; ----
;; #""body""
;; #"delimiter"body"delimiter"
;;
;; 参数
;; ----
;; body : text
;; 字面量中的原始文本，不做反斜杠转义解释。
;;
;; delimiter : string
;; 自定义分隔符，用于包裹带双引号或多行内容的文本。
;;
;; 返回值
;; ----
;; string
;; 读取后的 Scheme 字符串。
;;
;; 说明
;; ----
;; raw string 会原样保留内部字符，包括反斜杠、换行和双引号。
;; 当内容中可能出现默认结束模式时，可以改用自定义 delimiter。
;;
;; 错误处理
;; ----
;; 非法字面量格式时由 reader 层报错。

(check #"""" => "")
(check #"" "" => " ")
(check #""a"" => "a")
(check #""\"" => "\\")
(check #"-"""-" => "\"")
(check #"-"\""-" => "\\\"")
(check #"-"#"()""-" => "#\"()\"")
(check #"-"#""a"""-" => "#\"\"a\"\"")
(check #"-"ends with \""-" => "ends with \\\"")

(check
 #""multiline
string""
 => "multiline\nstring"
) ;check

(check
 #""
    no whitespace stripping""
 => "\n    no whitespace stripping"
) ;check

(check
 #""
    no whitespace stripping
  ""
 => "\n    no whitespace stripping\n  "
) ;check

(check
 #""
  注释 ;; comment
  ""
 => "\n  注释 ;; comment\n  "
) ;check

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
 => "\n<!DOCTYPE html>\n<html>\n  <head><title>\"测试页面\"</title></head>\n  <body>\n    <p>这里有很多\"引号\"</p>\n  </body>\n</html>\n  "
) ;check

(check
 #"HTML"<!DOCTYPE html>
<html>
  <head><title>"测试页面"</title></head>
  <body>
    <p>这里有很多"引号"</p>
  </body>
</html>
  "HTML"
 => "<!DOCTYPE html>\n<html>\n  <head><title>\"测试页面\"</title></head>\n  <body>\n    <p>这里有很多\"引号\"</p>\n  </body>\n</html>\n  "
) ;check

(check
 #"HTML"<!DOCTYPE html>
<html>
  <head><title>"测试页面"</title></head>
  <body>
    <p>这里有很多"引号"</p>
  </body>
</html>"HTML"
 => "<!DOCTYPE html>\n<html>\n  <head><title>\"测试页面\"</title></head>\n  <body>\n    <p>这里有很多\"引号\"</p>\n  </body>\n</html>"
) ;check

(check #"tag with space"hello"tag with space" => "hello")
(check #"(())"value"(())" => "value")

(check-report)
