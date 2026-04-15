;; (liii bitwise) 模块函数分类索引
;;
;; bitwise 提供按位逻辑、移位、位字段和位查询等操作。
;; 适合处理标志位、权限掩码、压缩状态和底层协议编码。


;; ==== 常见用法示例 ====
(import (liii bitwise))


;; 示例1：组合多个位掩码
(bitwise-and 12 10)


;; 示例2：通过移位快速调整数值
(arithmetic-shift 3 2)


;; 示例3：检查某一位是否被设置
(bit-set? 3 10)


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/bitwise "bitwise-and"
;;   bin/gf doc liii/bitwise "bit-set?"


;; ==== 函数分类索引 ====


;; 一、按位逻辑
;; 用于执行按位布尔运算的函数
;;   lognot             - 旧名，按位取反
;;   logand             - 旧名，按位与
;;   logior             - 旧名，按位或
;;   logxor             - 旧名，按位异或
;;   bitwise-not        - 按位取反
;;   bitwise-and        - 按位与
;;   bitwise-ior        - 按位或
;;   bitwise-xor        - 按位异或
;;   bitwise-eqv        - 按位等价
;;   bitwise-or         - 多参数按位或
;;   bitwise-nor        - 按位或非
;;   bitwise-nand       - 按位与非
;;   bitwise-orc1       - 对第一个参数取反后做按位或
;;   bitwise-orc2       - 对第二个参数取反后做按位或
;;   bitwise-andc1      - 对第一个参数取反后做按位与
;;   bitwise-andc2      - 对第二个参数取反后做按位与
;;   bitwise-if         - 根据掩码选择位


;; 二、移位与长度
;; 用于调整位布局和统计位长度的函数
;;   arithmetic-shift   - 进行算术移位
;;   ash                - arithmetic-shift 的别名
;;   integer-length     - 获取整数所需的位长度
;;   bit-count          - 统计置位个数


;; 三、位查询与单点修改
;; 用于查询或修改单个位的函数
;;   bit-set?           - 判断某一位是否为 1
;;   copy-bit           - 将某一位设置为指定值
;;   bit-swap           - 交换两个位
;;   any-bit-set?       - 判断掩码内是否存在置位
;;   every-bit-set?     - 判断掩码内是否全部置位
;;   first-set-bit      - 获取最低位的置位位置


;; 四、位字段操作
;; 用于处理连续位区间的函数
;;   bit-field          - 提取位字段
;;   bit-field-any?     - 判断位字段中是否存在置位
;;   bit-field-every?   - 判断位字段中是否全部置位
;;   bit-field-clear    - 清空位字段
;;   bit-field-set      - 设置位字段
