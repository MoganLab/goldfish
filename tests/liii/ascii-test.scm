;; (liii ascii) 模块函数分类索引
;;
;; ascii 提供面向 ASCII 字符、码点、字节向量和字符串的判定、转换与比较函数。
;; 适合协议解析、词法分析、命令行处理和只面向 ASCII 的文本逻辑。

;; ==== 常见用法示例 ====
(import (liii ascii))

;; 示例1：判断字符是否属于 ASCII 字母
(ascii-alphabetic? #\A) ; => #t

;; 示例2：把 ASCII 字符转换成数值或统一大小写
(ascii-digit-value #\F 16) ; => 15
(ascii-downcase #\G) ; => #\g

;; 示例3：进行大小写无关的字符串比较
(ascii-string-ci=? "GoldFish" "goldfish") ; => #t

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/ascii "ascii-alphabetic?"
;;   bin/gf doc liii/ascii "ascii-string-ci=?"

;; ==== 函数分类索引 ====

;; 一、码点与容器判定
;; 用于判断输入是否处于 ASCII 范围内的函数
;;   ascii-codepoint?     - 判断整数是否为 ASCII 码点
;;   ascii-bytevector?    - 判断字节向量是否全部为 ASCII
;;   ascii-char?          - 判断字符是否在 ASCII 范围内
;;   ascii-string?        - 判断字符串是否全部由 ASCII 字符组成

;; 二、字符类别判定
;; 用于判断 ASCII 字符类别的函数
;;   ascii-control?       - 判断是否为控制字符
;;   ascii-non-control?   - 判断是否为非控制字符
;;   ascii-whitespace?    - 判断是否为空白字符
;;   ascii-space-or-tab?  - 判断是否为空格或制表符
;;   ascii-other-graphic? - 判断是否为其他可显示符号
;;   ascii-upper-case?    - 判断是否为大写字母
;;   ascii-lower-case?    - 判断是否为小写字母
;;   ascii-alphabetic?    - 判断是否为字母
;;   ascii-alphanumeric?  - 判断是否为字母或数字
;;   ascii-numeric?       - 判断是否为数字

;; 三、转换函数
;; 用于在字符、数值和辅助符号之间转换的函数
;;   ascii-digit-value        - 把数字字符转换成数值
;;   ascii-upper-case-value   - 把大写字母转换成进制值
;;   ascii-lower-case-value   - 把小写字母转换成进制值
;;   ascii-nth-digit          - 获取第 n 个数字字符
;;   ascii-nth-upper-case     - 获取第 n 个大写字母
;;   ascii-nth-lower-case     - 获取第 n 个小写字母
;;   ascii-upcase             - 转成大写
;;   ascii-downcase           - 转成小写
;;   ascii-control->graphic   - 将控制字符映射为可视字符
;;   ascii-graphic->control   - 将可视字符映射为控制字符
;;   ascii-mirror-bracket     - 获取配对括号

;; 四、比较函数
;; 用于进行大小写无关比较的函数
;;   ascii-ci=?           - 判断两个字符是否大小写无关相等
;;   ascii-ci<?           - 判断两个字符是否大小写无关小于
;;   ascii-ci>?           - 判断两个字符是否大小写无关大于
;;   ascii-ci<=?          - 判断两个字符是否大小写无关小于等于
;;   ascii-ci>=?          - 判断两个字符是否大小写无关大于等于
;;   ascii-string-ci=?    - 判断两个字符串是否大小写无关相等
;;   ascii-string-ci<?    - 判断两个字符串是否大小写无关小于
;;   ascii-string-ci>?    - 判断两个字符串是否大小写无关大于
;;   ascii-string-ci<=?   - 判断两个字符串是否大小写无关小于等于
;;   ascii-string-ci>=?   - 判断两个字符串是否大小写无关大于等于

;; 五、括号辅助函数
;; 用于判断和处理配对括号的函数
;;   ascii-left-paren?    - 判断是否为左括号类字符
;;   ascii-right-paren?   - 判断是否为右括号类字符
