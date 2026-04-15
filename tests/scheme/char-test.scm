;; (scheme char) 模块函数分类索引
;;
;; char 提供字符大小写转换、字符类别判断和大小写无关比较等能力。
;; 它适合词法分析、文本规范化和基于字符属性的处理逻辑。
;; ==== 常见用法示例 ====
(import (scheme char))
;; 示例1：进行字符大小写转换
(char-upcase #\a)
(char-downcase #\Z)
;; 示例2：判断字符类别
(char-alphabetic? #\A)
(char-whitespace? #\space)
;; 示例3：做大小写无关比较和数字值转换
(char-ci=? #\a #\A)
(digit-value #\7)
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc scheme/char "char-upcase"
;;   bin/gf doc scheme/char "char-ci=?"
;; ==== 函数分类索引 ====
;; 一、字符大小写转换函数
;; 用于在不同字符大小写之间转换的函数
;;   char-upcase        - 将字符转换为大写
;;   char-downcase      - 将字符转换为小写
;;   char-foldcase      - 将字符转换为大小写无关的折叠形式
;; 二、字符类别判定
;; 用于判断字符属性的函数
;;   char-upper-case?   - 判断是否为大写字符
;;   char-lower-case?   - 判断是否为小写字符
;;   char-numeric?      - 判断是否为数字字符
;;   char-alphabetic?   - 判断是否为字母字符
;;   char-whitespace?   - 判断是否为空白字符
;; 三、字符大小写无关比较
;; 用于进行大小写无关字符比较的函数
;;   char-ci=?          - 判断两个字符是否大小写无关相等
;;   char-ci<?          - 判断两个字符是否大小写无关小于
;;   char-ci>?          - 判断两个字符是否大小写无关大于
;;   char-ci<=?         - 判断两个字符是否大小写无关小于等于
;;   char-ci>=?         - 判断两个字符是否大小写无关大于等于
;; 四、数字转换函数
;; 用于把字符转换为对应数字值的函数
;;   digit-value        - 获取数字字符对应的数值
;; 五、字符串大小写无关比较
;; 用于进行大小写无关字符串比较的函数
;;   string-ci=?        - 判断两个字符串是否大小写无关相等
;;   string-ci<?        - 判断两个字符串是否大小写无关小于
;;   string-ci>?        - 判断两个字符串是否大小写无关大于
;;   string-ci<=?       - 判断两个字符串是否大小写无关小于等于
;;   string-ci>=?       - 判断两个字符串是否大小写无关大于等于
;; 六、字符串大小写转换函数
;; 用于在不同字符串大小写之间转换的函数
;;   string-upcase      - 将字符串转换为大写
;;   string-downcase    - 将字符串转换为小写
;;   string-foldcase    - 将字符串转换为大小写无关的折叠形式