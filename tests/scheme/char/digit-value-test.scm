(import (liii check) (scheme char))
(check-set-mode! 'report-failed)

;; digit-value
;; 获取数字字符的数值
;;
;; 语法
;; ----
;; (digit-value char) → integer | #f
;;
;; 参数
;; ----
;; char : character
;; 要获取数值的字符
;;
;; 返回值
;; ------
;; integer | #f
;; 如果字符是数字字符，返回对应的整数值（0-9）；否则返回 #f
;;
;; 注意
;; ----
;; - 对于数字字符 #\0 到 #\9，返回对应的整数值 0 到 9
;; - 对于中文简体数字字符（〇、一、二、三、四、五、六、七、八、九），返回对应的整数值
;; - 对于中文繁体数字字符（零、壹、贰、叁、肆、伍、陆、柒、捌、玖），返回对应的整数值
;; - 对于日文数字字符（一、二、三、四、五、六、七、八、九、壱、弐、参、陸），返回对应的整数值
;; - 对于韩文数字字符（영、일、이、삼、사、오、육、칠、팔、구），返回对应的整数值
;; - 对于非数字字符，返回 #f
;;
;; 错误处理
;; ------
;; type-error
;; 参数必须是字符类型，否则会抛出异常

;; ASCII 数字字符测试
(check (digit-value #\0) => 0)
(check (digit-value #\1) => 1)
(check (digit-value #\2) => 2)
(check (digit-value #\3) => 3)
(check (digit-value #\4) => 4)
(check (digit-value #\5) => 5)
(check (digit-value #\6) => 6)
(check (digit-value #\7) => 7)
(check (digit-value #\8) => 8)
(check (digit-value #\9) => 9)

;; 非数字字符测试
(check (digit-value #\a) => #f)
(check (digit-value #\c) => #f)
(check (digit-value #\A) => #f)
(check (digit-value #\Z) => #f)
(check (digit-value #\space) => #f)
(check (digit-value #\newline) => #f)
(check (digit-value #\null) => #f)
(check (digit-value #\.) => #f)
(check (digit-value #\,) => #f)
(check (digit-value #\!) => #f)
(check (digit-value #\@) => #f)
(check (digit-value #\$) => #f)
(check (digit-value #\%) => #f)
(check (digit-value #\^) => #f)
(check (digit-value #\&) => #f)
(check (digit-value #\*) => #f)
(check (digit-value #\() => #f)
(check (digit-value #\)) => #f)
(check (digit-value #\_) => #f)
(check (digit-value #\+) => #f)
(check (digit-value #\-) => #f)
(check (digit-value #\=) => #f)
(check (digit-value #\[) => #f)
(check (digit-value #\]) => #f)
(check (digit-value #\{) => #f)
(check (digit-value #\}) => #f)
(check (digit-value #\|) => #f)
(check (digit-value #\\) => #f)
(check (digit-value #\:) => #f)
(check (digit-value #\;) => #f)
(check (digit-value #\") => #f)
(check (digit-value #\') => #f)
(check (digit-value #\<) => #f)
(check (digit-value #\>) => #f)
(check (digit-value #\?) => #f)
(check (digit-value #\/) => #f)
(check (digit-value #\x3007) => 0)
(check (digit-value #\x4E00) => 1)
(check (digit-value #\x4E8C) => 2)
(check (digit-value #\x4E09) => 3)
(check (digit-value #\x56DB) => 4)
(check (digit-value #\x4E94) => 5)
(check (digit-value #\x516D) => 6)
(check (digit-value #\x4E03) => 7)
(check (digit-value #\x516B) => 8)
(check (digit-value #\x4E5D) => 9)
(check (digit-value #\〇) => 0)
(check (digit-value #\一) => 1)
(check (digit-value #\二) => 2)
(check (digit-value #\三) => 3)
(check (digit-value #\四) => 4)
(check (digit-value #\五) => 5)
(check (digit-value #\六) => 6)
(check (digit-value #\七) => 7)
(check (digit-value #\八) => 8)
(check (digit-value #\九) => 9)
(check (digit-value #\x96F6) => 0)
(check (digit-value #\x58F9) => 1)
(check (digit-value #\x8D30) => 2)
(check (digit-value #\x53C1) => 3)
(check (digit-value #\x8086) => 4)
(check (digit-value #\x4F0D) => 5)
(check (digit-value #\x9646) => 6)
(check (digit-value #\x67D2) => 7)
(check (digit-value #\x634C) => 8)
(check (digit-value #\x7396) => 9)
(check (digit-value #\零) => 0)
(check (digit-value #\壹) => 1)
(check (digit-value #\贰) => 2)
(check (digit-value #\叁) => 3)
(check (digit-value #\肆) => 4)
(check (digit-value #\伍) => 5)
(check (digit-value #\陆) => 6)
(check (digit-value #\柒) => 7)
(check (digit-value #\捌) => 8)
(check (digit-value #\玖) => 9)
(check (digit-value #\x58F1) => 1)
(check (digit-value #\x5F10) => 2)
(check (digit-value #\x53C2) => 3)
(check (digit-value #\x9678) => 6)
(check (digit-value #\壱) => 1)
(check (digit-value #\弐) => 2)
(check (digit-value #\参) => 3)
(check (digit-value #\陸) => 6)
(check (digit-value #\xC601) => 0)
(check (digit-value #\xC77C) => 1)
(check (digit-value #\xC774) => 2)
(check (digit-value #\xC0BC) => 3)
(check (digit-value #\xC0AC) => 4)
(check (digit-value #\xC624) => 5)
(check (digit-value #\xC721) => 6)
(check (digit-value #\xCE60) => 7)
(check (digit-value #\xD314) => 8)
(check (digit-value #\xAD6C) => 9)
(check (digit-value #\영) => 0)
(check (digit-value #\일) => 1)
(check (digit-value #\이) => 2)
(check (digit-value #\삼) => 3)
(check (digit-value #\사) => 4)
(check (digit-value #\오) => 5)
(check (digit-value #\육) => 6)
(check (digit-value #\칠) => 7)
(check (digit-value #\팔) => 8)
(check (digit-value #\구) => 9)
(check (digit-value #\x3007) => 0)
(check (digit-value #\x20B20) => 1)
(check (digit-value #\x20129) => 2)
(check (digit-value #\x20027) => 3)
(check (digit-value #\x2629A) => 4)
(check (digit-value #\x2013C) => 5)
(check (digit-value #\x264B9) => 6)
(check (digit-value #\x26271) => 7)
(check (digit-value #\x20969) => 8)
(check (digit-value #\x200E9) => 9)
(check (digit-value #\〇) => 0)
(check (digit-value #\𠬠) => 1)
(check (digit-value #\𠄩) => 2)
(check (digit-value #\𠀧) => 3)
(check (digit-value #\𦊚) => 4)
(check (digit-value #\𠄼) => 5)
(check (digit-value #\𦒹) => 6)
(check (digit-value #\𦉱) => 7)
(check (digit-value #\𠥩) => 8)
(check (digit-value #\𠃩) => 9)
(check-report)
