# (liii list) 模块函数分类索引

`(liii list)` 提供面向 Scheme 列表的扩展操作，包括宽容边界切片、长度判断、嵌套展平与映射等实用工具函数。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/list "函数名"
```

## 函数分类索引

### 一、宽容切片与截取
- `list-take` - 边界宽容版取前 n 个元素
- `list-drop` - 边界宽容版跳过前 n 个元素
- `list-take-right` - 边界宽容版取后 n 个元素
- `list-drop-right` - 边界宽容版跳过后 n 个元素

### 二、长度与空判断
- `length=?` - 判断长度是否等于给定值
- `length>?` - 判断长度是否大于给定值
- `length>=?` - 判断长度是否大于等于给定值
- `list-null?` - 判断是否为空列表
- `list-not-null?` - 判断是否为非空列表
- `not-null-list?` - 判断是否为非空真列表

### 三、展平与映射
- `flatten` - 拍平嵌套列表
- `flat-map` - 映射并扁平化列表

### 四、关联列表
- `alist-cons` - 在关联列表前端插入一个新的键值对
