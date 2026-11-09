# (liii string-cursor) 模块函数分类索引

`string-cursor` 在字符串遍历中引入「游标」（cursor）抽象：
游标表示字符串中的一个位置，可用于随机访问、切片与区间遍历。

与基于整数索引的 `(liii string)` 相比，游标版本：

- 统一了「索引」与「游标」两类参数，`start`/`end` 既可以写整数索引，也可以写游标
- 以 Unicode 字符为粒度遍历，对多字节字符（如中文、emoji）处理更精确
- 提供 `string-cursor-next`/`string-cursor-prev`/`string-cursor-forward`/`string-cursor-back`
  等游标移动原语，便于构建区间扫描算法

## 常见用法示例

```scheme
(import (liii string-cursor))

;; 示例1：创建游标并比较位置
(define s "abcdef")
(string-cursor<=? (string-index->cursor s 0) (string-index->cursor s 3)) ; => #t

;; 示例2：计算两个游标之间的距离
(string-cursor-diff s (string-cursor-start s) (string-cursor-end s)) ; => 6

;; 示例3：按游标切分字符串
(substring/cursors s (string-index->cursor s 1) (string-index->cursor s 4)) ; => "bcd"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/string-cursor "函数名"
```

## 函数分类索引

### 一、游标操作
- `string-cursor?` - 判断对象是否为字符串游标
- `string-cursor-start` / `string-cursor-end` - 取字符串起始 / 结束游标
- `string-cursor-next` / `string-cursor-prev` - 取下一个 / 上一个游标
- `string-cursor-forward` / `string-cursor-back` - 向前 / 向后移动若干个字符
- `string-cursor=?` / `string-cursor<?` / `string-cursor>?` / `string-cursor<=?` / `string-cursor>=?` - 比较游标位置
- `string-cursor-diff` - 计算两个游标之间的字符距离
- `string-cursor->index` / `string-index->cursor` - 游标与索引互转

### 二、谓词
- `string-null?` - 判断字符串是否为空
- `string-every` / `string-any` - 判断是否所有 / 存在字符满足条件

### 三、构造函数
- `string-tabulate` - 按字符生成过程构造字符串
- `string-unfold` / `string-unfold-right` - 通过 unfold 规则构造字符串

### 四、类型转换
- `string->list/cursors` - 转换为游标列表
- `string->vector/cursors` - 转换为游标向量
- `reverse-list->string` - 由字符列表构造字符串
- `string-join` - 连接字符串列表

### 五、选择与切片
- `string-ref/cursor` - 按游标取字符
- `substring/cursors` - 按游标区间取子串
- `string-copy/cursors` - 按游标区间复制字符串
- `string-take` / `string-take-right` - 取前 n 个 / 后 n 个字符
- `string-drop` / `string-drop-right` - 跳过前 n 个 / 后 n 个字符
- `string-pad` / `string-pad-right` - 向左 / 向右填充字符串
- `string-trim` / `string-trim-right` / `string-trim-both` - 修整字符串

### 六、前后缀
- `string-prefix-length` / `string-suffix-length` - 取公共前缀 / 后缀长度
- `string-prefix?` / `string-suffix?` - 判断是否以指定前缀 / 后缀开头或结尾

### 七、查找
- `string-index` / `string-index-right` - 查找满足条件的位置
- `string-skip` / `string-skip-right` - 跳过满足条件的字符
- `string-contains` / `string-contains-right` - 判断是否包含子串

### 八、整体操作
- `string-reverse` - 反转字符串
- `string-concatenate` / `string-concatenate-reverse` - 拼接 / 反向拼接字符串
- `string-fold` / `string-fold-right` - 左折叠 / 右折叠
- `string-for-each-cursor` - 按游标遍历字符
- `string-replicate` - 重复字符串
- `string-count` - 统计满足条件的字符个数
- `string-replace` - 替换子串
- `string-split` - 按分隔符切分字符串
- `string-filter` / `string-remove` - 保留 / 移除满足条件的字符
