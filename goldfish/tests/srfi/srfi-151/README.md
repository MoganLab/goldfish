# (srfi srfi-151) 模块函数分类索引

SRFI-151 是 Scheme 通用位运算库（Bitwise Techniques），提供了按位逻辑、移位、位字段和位查询等操作。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc srfi/srfi-151 "bitwise-and"
```
或者直接按函数名检索：
```bash
gf doc "bitwise-and"
```

## 函数分类索引

### 一、按位逻辑
- `bitwise-not` - 按位取反
- `bitwise-and` - 按位与
- `bitwise-ior` - 按位或
- `bitwise-xor` - 按位异或
- `bitwise-eqv` - 按位等价
- `bitwise-nor` - 按位或非
- `bitwise-nand` - 按位与非
- `bitwise-orc1` - 对第一个参数取反后做按位或
- `bitwise-orc2` - 对第二个参数取反后做按位或
- `bitwise-andc1` - 对第一个参数取反后做按位与
- `bitwise-andc2` - 对第二个参数取反后做按位与
- `bitwise-if` - 根据掩码选择位

### 二、移位与长度
- `arithmetic-shift` - 进行算术移位
- `integer-length` - 获取整数所需的位长度
- `bit-count` - 统计置位个数

### 三、位查询与单点修改
- `bit-set?` - 判断某一位是否为 1
- `copy-bit` - 将某一位设置为指定值
- `bit-swap` - 交换两个位
- `any-bit-set?` - 判断掩码内是否存在置位
- `every-bit-set?` - 判断掩码内是否全部置位
- `first-set-bit` - 获取最低位的置位位置

### 四、位字段操作
- `bit-field` - 提取位字段
- `bit-field-any?` - 判断位字段中是否存在置位
- `bit-field-every?` - 判断位字段中是否全部置位
- `bit-field-clear` - 清空位字段
- `bit-field-set` - 设置位字段
