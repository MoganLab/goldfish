;; (liii string) 模块函数分类索引
;;
;; string 提供常见字符串查询、切片、变换、折叠和分词能力。
;; 它补齐了 Goldfish 在文本处理上的高频接口，适合命令行、配置和轻量解析场景。

;; ==== 常见用法示例 ====
(import (liii string))

;; 示例1：判断字符串是否以某个前缀开头
(string-starts? "goldfish" "gold") ; => #t

;; 示例2：把字符串按分隔符切分
(string-split "a,b,c" ",") ; => ("a" "b" "c")

;; 示例3：去掉字符串两端空白
(string-trim "  hello  ") ; => "hello"

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/string "string-split"
;;   bin/gf doc liii/string "string-trim"

;; ==== 函数分类索引 ====

;; 一、基本谓词与访问
;; 用于判断和读取字符串内容的函数
;;   string?              - 判断对象是否为字符串
;;   string-ref           - 按索引读取字符
;;   string-length        - 获取字符串长度
;;   string-copy          - 复制字符串
;;   string-null?         - 判断字符串是否为空
;;   string-contains      - 查找子串位置
;;   string-contains?     - 判断是否包含子串
;;   string-index         - 查找满足条件的首个位置
;;   string-index-right   - 从右侧查找满足条件的位置

;; 二、切片与边界处理
;; 用于截取、填充和修整字符串的函数
;;   string-take          - 取前 n 个字符
;;   string-take-right    - 取后 n 个字符
;;   string-drop          - 跳过前 n 个字符
;;   string-drop-right    - 跳过后 n 个字符
;;   string-pad           - 向左补齐字符串
;;   string-pad-right     - 向右补齐字符串
;;   string-trim          - 去掉左侧满足条件的字符
;;   string-trim-right    - 去掉右侧满足条件的字符
;;   string-trim-both     - 去掉两侧满足条件的字符
;;   string-remove-prefix - 移除前缀
;;   string-remove-suffix - 移除后缀

;; 三、转换与替换
;; 用于变换字符串内容的函数
;;   string-upcase        - 转为大写
;;   string-downcase      - 转为小写
;;   string-reverse       - 反转字符串
;;   string-join          - 连接字符串列表
;;   string-split         - 按分隔符切分字符串
;;   string-replace       - 替换子串
;;   string-tokenize      - 按分隔规则拆分为 token

;; 四、迭代与归约
;; 用于遍历、映射和折叠字符串的函数
;;   string-map           - 映射字符
;;   string-for-each      - 遍历字符
;;   string-for-each-index - 带索引遍历字符
;;   string-fold          - 左折叠
;;   string-fold-right    - 右折叠
;;   string-any           - 判断是否存在满足条件的字符
;;   string-every         - 判断是否所有字符都满足条件

;; 五、前后缀与匹配
;; 用于判断前后缀和统计匹配区间的函数
;;   string-starts?       - 判断是否以指定前缀开头
;;   string-ends?         - 判断是否以指定后缀结尾
;;   string-skip          - 跳过前方满足条件的字符
;;   string-skip-right    - 从右侧跳过满足条件的字符
;;   string-count         - 统计满足条件的字符个数
