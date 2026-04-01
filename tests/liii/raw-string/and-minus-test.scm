(import (liii check)
        (liii raw-string)
) ;import

(check-set-mode! 'report-failed)

;; &-
;; deindent 的别名，功能完全相同。
;;
;; 语法
;; ----
;; (&- #"" ... "")
;; (&- #"TAG" ... "TAG")
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
;; &- 是 deindent 的简短别名，提供相同的功能。
;; 它更适合在代码中频繁使用时减少输入。
;;
;; 使用建议：
;; - &- 适合快速、简短的字符串处理
;; - deindent 适合在需要明确语义的场合

;; 基本功能（与 deindent 相同）
(check
 (&- #""
  第一行
  第二行
  第三行
  ""
 ) ;&-
 => "第一行\n第二行\n第三行"
) ;check

;; 带缩进
(check
 (&- #""
  外层
    内层
  外层结束
  ""
 ) ;&-
 => "外层\n  内层\n外层结束"
) ;check

;; 使用自定义标签
(check
 (&- #"SQL"
  SELECT id, name
  FROM users
  WHERE status = 'active'
  "SQL"
 ) ;&-
 => "SELECT id, name\nFROM users\nWHERE status = 'active'"
) ;check

;; HTML 代码块
(check
 (&- #"HTML"
  <html>
    <head>
      <title>测试</title>
    </head>
  </html>
  "HTML"
 ) ;&-
 => "<html>\n  <head>\n    <title>测试</title>\n  </head>\n</html>"
) ;check

(check-report)
