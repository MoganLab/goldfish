;; (liii string) 模块函数分类索引
;;
;; string 提供常见字符串查询、切片、变换、折叠和分词能力。
;; 它补齐了 Goldfish 在文本处理上的高频接口，适合命令行、配置和轻量解析场景。
;;
;; 本模块导出的函数来源于：
;; - (srfi srfi-13): string-null?, string-join, string-every, string-any, string-take,
;;                   string-take-right, string-drop, string-drop-right, string-pad,
;;                   string-pad-right, string-trim, string-trim-right, string-trim-both,
;;                   string-index, string-index-right, string-skip, string-skip-right,
;;                   string-contains, string-count, string-upcase, string-downcase,
;;                   string-fold, string-fold-right, string-for-each-index, string-reverse,
;;                   string-tokenize
;; - Liii 扩展: string-starts?, string-ends?, string-contains?, string-split, string-replace,
;;             string-remove-prefix, string-remove-suffix


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/string "string-split"
;;   bin/gf doc liii/string "string-trim"
;;
;; 相关库文档：
;;   bin/gf doc srfi/srfi-13   - 查看 SRFI-13 字符串库
;;   bin/gf doc scheme/base    - 查看 scheme base 库


;; ==== 函数分类索引 ====


;; 一、基本谓词与访问
;; 用于判断和读取字符串内容的函数
;;   string-null?         - 判断字符串是否为空
;;   string-contains      - 判断是否包含子串（推荐使用 string-contains?）
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
;;   string-for-each-index - 带索引遍历字符
;;   string-fold          - 左折叠
;;   string-fold-right    - 右折叠
;;   string-any           - 判断是否存在满足条件的字符
;;   string-every         - 判断是否所有字符都满足条件


;; 五、前后缀与匹配
;; 用于判断前后缀和统计匹配区间的函数
;;   string-starts?       - 判断是否以指定前缀开头（推荐）
;;   string-ends?         - 判断是否以指定后缀结尾（推荐）
;;   string-skip          - 跳过前方满足条件的字符
;;   string-skip-right    - 从右侧跳过满足条件的字符
;;   string-count         - 统计满足条件的字符个数
;;
;; 注意：string-starts? 和 string-ends? 比 string-prefix? 和 string-suffix? 更推荐使用，
;; 因为参数顺序更直观：源字符串始终在前，模式在后。
;;   (string-starts? "goldfish" "gold")  ; 直观：检查 "goldfish" 是否以 "gold" 开头
;;   (string-prefix? "gold" "goldfish")  ; 不直观：参数顺序相反
