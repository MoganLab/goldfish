;; (scheme base) 模块函数分类索引
;;
;; R7RS `scheme base` 是 Goldfish Scheme 的核心基础库，
;; 提供了 Scheme 语言最基本的数据类型、控制结构和 I/O 操作。

;; ==== 扩展库 ====
;; Goldfish 提供了 `(liii base)` 作为 `scheme base` 的扩展，
;; 包含额外的实用函数。如需使用，请导入：
;;   (import (liii base))
;;
;; 查看扩展库的完整文档：
;;   bin/gf doc liii/base

;; ==== 函数分类索引 ====

;; 一、数值运算 (R7RS 6.2 Numbers)
;;   +                    - 加法
;;   -                    - 减法/取负
;;   *                    - 乘法
;;   /                    - 除法
;;   =                    - 数值相等
;;   <                    - 小于
;;   >                    - 大于
;;   <=                   - 小于等于
;;   >=                   - 大于等于
;;   abs                  - 绝对值
;;   max                  - 最大值
;;   min                  - 最小值
;;   modulo               - 模运算
;;   quotient             - 整除商
;;   remainder            - 整除余数
;;   gcd                  - 最大公约数
;;   lcm                  - 最小公倍数
;;   floor                - 向下取整
;;   ceiling              - 向上取整
;;   truncate             - 向零截断
;;   round                - 四舍五入
;;   exact                - 转换为精确数
;;   inexact              - 转换为非精确数
;;   exact-integer-sqrt   - 精确整数平方根
;;   numerator            - 有理数分子
;;   denominator          - 有理数分母
;;   number->string       - 数字转字符串
;;   string->number       - 字符串转数字

;; 二、类型判断 (R7RS 6.1 Equivalence predicates + 类型判断)
;;   number?              - 是否为数值
;;   integer?             - 是否为整数
;;   real?                - 是否为实数
;;   rational?            - 是否为有理数
;;   complex?             - 是否为复数
;;   exact-integer?       - 是否为精确整数
;;   inexact?             - 是否为非精确数
;;   exact?               - 是否为精确数
;;   positive?            - 是否为正数
;;   negative?            - 是否为负数
;;   zero?                - 是否为零
;;   even?                - 是否为偶数
;;   odd?                 - 是否为奇数
;;   pair?                - 是否为序对
;;   list?                - 是否为列表
;;   null?                - 是否为空列表
;;   symbol?              - 是否为符号
;;   string?              - 是否为字符串
;;   char?                - 是否为字符
;;   vector?              - 是否为向量
;;   bytevector?          - 是否为字节向量
;;   boolean?             - 是否为布尔值
;;   port?                - 是否为端口
;;   input-port?          - 是否为输入端口
;;   output-port?         - 是否为输出端口
;;   input-port-open?     - 输入端口是否打开
;;   output-port-open?    - 输出端口是否打开
;;   eof-object?          - 是否为文件结束对象
;;   file-error?          - 是否为文件错误
;;   read-error?          - 是否为读取错误

;; 三、序对与列表操作 (R7RS 6.4 Pairs and lists)
;;   cons                 - 构造序对
;;   car                  - 取序对首部
;;   cdr                  - 取序对尾部
;;   set-car!             - 修改序对首部
;;   set-cdr!             - 修改序对尾部
;;   caar                 - car 的 car
;;   cadr                 - car 的 cdr
;;   cddr                 - cdr 的 cdr
;;   list                 - 构造列表
;;   make-list            - 创建指定长度列表
;;   length               - 列表长度
;;   append               - 连接列表
;;   reverse              - 反转列表
;;   list-tail            - 取列表尾部
;;   list-ref             - 按索引取元素
;;   list-set!            - 按索引设置元素
;;   list-copy            - 复制列表
;;   memq                 - 按 eq? 查找
;;   memv                 - 按 eqv? 查找
;;   member               - 按 equal? 查找
;;   assq                 - 按 eq? 在关联列表中查找
;;   assv                 - 按 eqv? 在关联列表中查找
;;   assoc                - 按 equal? 在关联列表中查找

;; 四、符号 (R7RS 6.5 Symbols)
;;   symbol->string       - 符号转字符串
;;   string->symbol       - 字符串转符号
;;   symbol=?             - 符号相等判断

;; 五、字符 (R7RS 6.6 Characters)
;;   digit-value          - 字符数字值
;;   char->integer        - 字符转整数
;;   integer->char        - 整数转字符
;;   char?                - 是否为字符
;;   char=?               - 字符相等
;;   char<?               - 字符小于
;;   char>?               - 字符大于
;;   char<=?              - 字符小于等于
;;   char>=?              - 字符大于等于

;; 六、字符串 (R7RS 6.7 Strings)
;;   string?              - 是否为字符串
;;   make-string          - 创建指定长度字符串
;;   string               - 将字符列表转为字符串
;;   string-length        - 字符串长度
;;   string-ref           - 按索引取字符
;;   string-set!          - 按索引设置字符
;;   string-copy          - 复制字符串
;;   string-append        - 连接多个字符串
;;   substring            - 提取子字符串
;;   string-fill!         - 用字符填充字符串
;;   string->list         - 字符串转字符列表
;;   list->string         - 字符列表转字符串
;;   string=?             - 字符串相等比较
;;   string<?             - 字符串小于比较
;;   string>?             - 字符串大于比较
;;   string<=?            - 字符串小于等于比较
;;   string>=?            - 字符串大于等于比较

;; 七、向量 (R7RS 6.8 Vectors)
;;   make-vector          - 创建向量
;;   vector               - 构造向量
;;   vector-length        - 向量长度
;;   vector-ref           - 按索引取向量元素
;;   vector-set!          - 按索引设置向量元素
;;   vector-map           - 向量映射
;;   vector->string       - 向量转字符串
;;   string->vector       - 字符串转向量
;;   vector-copy          - 复制向量
;;   vector-copy!         - 复制向量到另一向量
;;   vector-fill!         - 填充向量

;; 八、字节向量 (R7RS 6.9 Bytevectors)
;;   make-bytevector      - 创建字节向量
;;   bytevector           - 构造字节向量
;;   bytevector-length    - 字节向量长度
;;   bytevector-u8-ref    - 按索引取字节
;;   bytevector-u8-set!   - 按索引设置字节
;;   bytevector-copy      - 复制字节向量
;;   bytevector-append    - 连接字节向量
;;   utf8->string         - UTF-8 转字符串
;;   string->utf8         - 字符串转 UTF-8
;;   utf8-string-length   - UTF-8 字符串长度
;;   bytevector-advance-utf8 - UTF-8 字节向量前进

;; 九、控制流 (R7RS 4.2 Derived conditionals + 4.2 Binding constructs)
;;   if                   - 条件分支
;;   cond                 - 多条件分支
;;   case                 - 模式匹配分支
;;   and                  - 逻辑与
;;   or                   - 逻辑或
;;   when                 - 条件执行
;;   unless               - 否定条件执行
;;   do                   - 循环
;;   let                  - 局部绑定
;;   let*                 - 顺序局部绑定
;;   letrec               - 递归局部绑定
;;   letrec*              - 顺序递归局部绑定
;;   let-values           - 多值绑定
;;   define-values        - 定义多值
;;   lambda               - 创建过程
;;   define-record-type   - 定义记录类型
;;   begin                - 顺序执行

;; 十、等价判断 (R7RS 6.1 Equivalence predicates)
;;   eq?                  - 相同对象判断
;;   eqv?                 - 等价判断
;;   equal?               - 结构相等判断
;;   not                  - 逻辑非
;;   boolean=?            - 布尔值相等

;; 十一、I/O 操作 (R7RS 6.13 Input and output)
;;   call-with-port       - 端口过程调用
;;   open-input-string    - 打开字符串输入端口
;;   open-output-string   - 打开字符串输出端口
;;   get-output-string    - 获取输出字符串
;;   close-port           - 关闭端口
;;   read                 - 读取对象
;;   read-line            - 读取一行
;;   write                - 写入对象

;; 十二、异常处理 (R7RS 6.11 Exceptions)
;;   raise                - 抛出异常
;;   guard                - 捕获并处理异常
;;   error                - 报错

