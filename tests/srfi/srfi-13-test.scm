;; (srfi srfi-13) 模块函数分类索引
;;
;; srfi-13 是字符串处理库，提供了丰富的字符串操作函数。
;; 它包含了字符串谓词、构造器、选择器、比较、裁剪、查找、删除、替换、过滤、折叠等功能。
;; 本实现基于 SRFI-13 标准规范。

;; ==== 常见用法示例 ====
(import (srfi srfi-13))

;; 示例1：判断字符串是否为空
(string-null? "") ; => #t
(string-null? "hello") ; => #f

;; 示例2：连接字符串列表
(string-join '("a" "b" "c") ",") ; => "a,b,c"

;; 示例3：取字符串前n个字符
(string-take "hello" 2) ; => "he"
;; 示例4：去掉字符串左侧空白
(string-trim "  hello  ") ; => "hello  "
(string-trim "  hello  ") ; => "hello"

;; 示例5：查找子串位置
(string-contains "hello world" "world") ; => #t

;; ==== 如何查看函数的文档和用例 ====
;;
;; 注意：目前只有以下两个函数的测试文档位于 tests/srfi/srfi-13/ 目录：
;;   bin/gf doc srfi/srfi-13 "string-prefix?"
;;   bin/gf doc srfi/srfi-13 "string-suffix?"
;;
;; 其他函数的测试文档位于 tests/liii/string/ 目录，推荐使用：
;;   bin/gf doc liii/string "string-null?"
;;   bin/gf doc liii/string "string-join"
;;   bin/gf doc liii/string "string-take"
;;   ...

;; ==== 函数分类索引 ====

;; 一、基本谓词与构造
;; 用于判断字符串状态和构造新字符串的函数
;;   string-null?    - 判断字符串是否为空
;;   string-copy     - 复制字符串（或其部分）
;;   string-join     - 连接字符串列表

;; 二、字符串选择器（切片）
;; 用于截取字符串特定部分的函数
;;   string-take          - 取前 n 个字符
;;   string-take-right    - 取后 n 个字符
;;   string-drop          - 跳过前 n 个字符
;;   string-drop-right    - 跳过后 n 个字符

;; 三、字符串填充与修整
;; 用于填充、修整字符串的函数
;;   string-pad           - 向左填充字符串到指定长度
;;   string-pad-right     - 向右填充字符串到指定长度
;;   string-trim          - 去掉左侧满足条件的字符
;;   string-trim-right    - 去掉右侧满足条件的字符
;;   string-trim-both     - 去掉两侧满足条件的字符

;; 四、字符串前缀与后缀
;; 用于判断前后缀的函数
;;   string-prefix?       - 判断字符串是否以指定前缀开始
;;   string-suffix?       - 判断字符串是否以指定后缀结束

;; 五、字符串查找与搜索
;; 用于在字符串中查找内容的函数
;;   string-index         - 查找满足条件的第一个位置
;;   string-index-right   - 从右侧查找满足条件的位置
;;   string-skip          - 跳过满足条件的字符，返回第一个不满足的位置
;;   string-skip-right    - 从右侧跳过满足条件的字符
;;   string-contains      - 判断字符串是否包含子串
;;   string-count         - 统计满足条件的字符个数

;; 六、字符串大小写转换
;; 用于转换字符串大小写的函数
;;   string-upcase        - 转为大写
;;   string-downcase      - 转为小写

;; 七、字符串反转与分词
;; 用于反转字符串和分词的函数
;;   string-reverse       - 反转字符串
;;   string-tokenize      - 按分隔符拆分字符串为列表

;; 八、字符串折叠与迭代
;; 用于折叠、遍历字符串的函数
;;   string-fold          - 左折叠字符串
;;   string-fold-right    - 右折叠字符串
;;   string-for-each-index - 带索引遍历字符
;;   string-any           - 判断是否存在满足条件的字符
;;   string-every         - 判断是否所有字符都满足条件
