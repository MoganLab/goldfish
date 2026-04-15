(import (liii check)
  (liii unicode)
  (liii base)
) ;import


(check-set-mode! 'report-failed)


;; unicode-replacement-char
;; Unicode 替换字符常量。
;;
;; 语法
;; ----
;; unicode-replacement-char
;;
;; 返回值
;; ----
;; integer
;; 返回 Unicode 替换字符的码点值 #xFFFD（十进制 65533）。
;;
;; 描述
;; ----
;; 替换字符（U+FFFD）用于替换无法识别的字符或无效的字节序列。
;; 它在处理损坏的编码数据时特别有用。


;; 基本值测试
(check unicode-replacement-char
  =>
  65533
) ;check
(check unicode-replacement-char
  =>
  65533
) ;check


;; 验证类型和值
(check-true (integer? unicode-replacement-char)
) ;check-true


;; 与 hexstr 转换
(check (codepoint->hexstr unicode-replacement-char
       ) ;codepoint->hexstr
  =>
  "FFFD"
) ;check
(check (hexstr->codepoint "FFFD")
  =>
  unicode-replacement-char
) ;check


(check-report)
