;; (liii base) 模块函数分类索引
;;
;; base 是 Goldfish 的基础库入口，承接 R7RS `scheme base`、部分 SRFI，
;; 以及若干常用扩展函数与语法工具，是大多数模块的底层依赖。

;; ==== 常见用法示例 ====
(import (liii base))

;; 示例1：进行常见数值运算
(square 12) ; => 144

;; 示例2：一次接收多个返回值
(receive (q r) (floor/ 17 5) (list q r)) ; => (3 2)

;; 示例3：处理 UTF-8 字符串
(utf8-string-length "你好，世界") ; => 5

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/base "square"
;;   bin/gf doc liii/base "receive"

;; ==== 函数分类索引 ====

;; 一、数值运算
;; 用于执行基础算术和数值转换的函数
;;   square               - 计算平方
;;   exact                - 转换为精确数
;;   inexact              - 转换为非精确数
;;   floor                - 向下取整
;;   ceiling              - 向上取整
;;   truncate             - 向零截断
;;   round                - 四舍五入
;;   gcd                  - 计算最大公约数
;;   lcm                  - 计算最小公倍数
;;   modulo               - 计算模
;;   exact-integer-sqrt   - 计算精确整数平方根
;;   numerator            - 获取有理数分子
;;   denominator          - 获取有理数分母
;;   number->string       - 把数字转换成字符串
;;   string->number       - 把字符串转换成数字

;; 二、基础数据结构
;; 用于构造和访问基础容器的函数
;;   pair?                - 判断是否为 pair
;;   list?                - 判断是否为列表
;;   symbol?              - 判断是否为符号
;;   string?              - 判断是否为字符串
;;   vector?              - 判断是否为向量
;;   bytevector?          - 判断是否为字节向量
;;   cons                 - 构造 pair
;;   car                  - 获取 pair 的首元素
;;   cdr                  - 获取 pair 的尾部
;;   list                 - 构造列表
;;   append               - 连接列表
;;   reverse              - 反转列表
;;   list-ref             - 按索引访问列表
;;   vector-ref           - 按索引访问向量
;;   bytevector-u8-ref    - 按索引访问字节向量

;; 三、字符与字符串工具
;; 用于处理字符编码和字符串内容的函数
;;   digit-value              - 获取字符对应的数字值
;;   string-copy              - 复制字符串
;;   utf8->string             - UTF-8 字节向量转字符串
;;   string->utf8             - 字符串转 UTF-8 字节向量
;;   utf8-string-length       - 获取 UTF-8 字符串长度
;;   u8-substring             - 截取 UTF-8 子串
;;   bytevector-advance-utf8  - 按 UTF-8 编码前进字节偏移

;; 四、控制流与 I/O
;; 用于处理多返回值、端口和异常的函数与语法
;;   let-values           - 解构多个返回值
;;   define-values        - 定义多个值
;;   define-record-type   - 定义记录类型
;;   call-with-port       - 以端口为参数执行过程
;;   open-input-string    - 从字符串创建输入端口
;;   open-output-string   - 创建输出字符串端口
;;   get-output-string    - 读取输出字符串端口的内容
;;   read                 - 从输入流读取对象
;;   write                - 将对象写入输出流
;;   raise                - 抛出异常
;;   guard                - 捕获并处理异常
;;   receive              - 接收多个返回值

;; 五、常用扩展工具
;; 用于简化函数组合和日常编程的扩展函数
;;   loose-car            - 宽松地获取 car
;;   loose-cdr            - 宽松地获取 cdr
;;   compose              - 组合多个函数
;;   identity             - 原样返回输入值
;;   any?                 - 判断是否存在真值
;;   let1                 - 单绑定 let 语法糖
;;   typed-lambda         - 带类型标注的 lambda 语法
