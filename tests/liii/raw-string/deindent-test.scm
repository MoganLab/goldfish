(import (liii check)
        (liii raw-string)
) ;import

(check-set-mode! 'report-failed)

;; deindent
;; 对多行字符串按 closing line 的缩进去掉公共前导空格。
;;
;; 语法
;; ----
;; (deindent string-literal)
;;
;; 参数
;; ----
;; string-literal : string
;; 以换行开头、以 closing line 缩进结尾的字符串字面量。
;;
;; 返回值
;; ----
;; string
;; 去掉公共缩进后的多行字符串。
;;
;; 说明
;; ----
;; deindent 在宏展开期处理字符串字面量，规则与 `(liii raw-string)` 中 `&-` 一致。
;; closing line 的前导空格既定义了基准缩进，也不会出现在最终结果里。
;;
;; 错误处理
;; ----
;; value-error
;; 字符串不是以新行开头、closing line 非法，或某一行缩进少于基准缩进时抛出。

(check
 (deindent "\n  第一行\n  第二行\n  第三行\n  ")
 => "第一行\n第二行\n第三行"
) ;check

(check
 (deindent "\n  外层\n    内层1\n      更内层\n    内层2\n  外层结束\n  ")
 => "外层\n  内层1\n    更内层\n  内层2\n外层结束"
) ;check

(check
 (deindent "\n  第一行\n\n  第三行（上下有空行）\n\n  第五行（上有空行）\n  ")
 => "第一行\n\n第三行（上下有空行）\n\n第五行（上有空行）"
) ;check

(check
 (deindent "\n  行尾有空格   \n  行尾有多个空格     \n  正常行\n  ")
 => "行尾有空格   \n行尾有多个空格     \n正常行"
) ;check

(check
 (deindent "\n    这行前面有0个空格后面也有4个空格    \n    这行前面有0个空格\n      这行前面有2个空格\n  ")
 => "  这行前面有0个空格后面也有4个空格    \n  这行前面有0个空格\n    这行前面有2个空格"
) ;check

(check
 (deindent "\n\n  ")
 => ""
) ;check

(check-catch 'value-error (deindent "第一行\n  "))
(check-catch 'value-error (deindent "\n"))
(check-catch 'value-error (deindent "\n  第一行\n\t第二行\n  "))
(check-catch 'value-error (deindent "\n  第一行\n 第二行\n  "))

(check-report)
