# (srfi srfi-1) 模块函数分类索引

SRFI-1 是 Scheme 列表处理库（List Library），提供了全面的列表构造、选择、遍历、过滤与折叠等高阶操作。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-1 "函数名"
```

## 函数分类索引

### 一、构造器与谓词
- `xcons` - 交换参数顺序构造点对
- `cons*` - 嵌套构造点对与列表
- `circular-list` - 构造循环列表
- `circular-list?` - 判断是否为循环列表
- `proper-list?` - 判断是否为正规列表
- `dotted-list?` - 判断是否为点列表
- `null-list?` - 判断是否为空列表

### 二、位置选择器
- `first` - 获取第 1 个元素
- `second` - 获取第 2 个元素
- `third` - 获取第 3 个元素
- `fourth` - 获取第 4 个元素
- `fifth` - 获取第 5 个元素
- `sixth` - 获取第 6 个元素
- `seventh` - 获取第 7 个元素
- `eighth` - 获取第 8 个元素
- `ninth` - 获取第 9 个元素
- `tenth` - 获取第 10 个元素
- `last` - 获取最后一个元素
- `last-pair` - 获取最后一个点对

### 三、切片与拆分
- `take` - 从列表开头提取指定数量元素
- `drop` - 从列表开头删除指定数量元素
- `take-right` - 从列表末尾提取指定数量元素
- `drop-right` - 从列表末尾删除指定数量元素
- `split-at` - 在指定位置拆分列表
- `take-while` - 提取满足谓词的前缀元素
- `drop-while` - 删除满足谓词的前缀元素

### 四、折叠与归约
- `fold` - 左折叠
- `fold-right` - 右折叠
- `reduce` - 左归约
- `reduce-right` - 右归约

### 五、映射、过滤与分区
- `append-map` - 映射并连接结果列表
- `filter` - 保留满足谓词的元素
- `remove` - 移除满足谓词的元素
- `partition` - 根据谓词将元素分区为两个列表
- `zip` - 组合多个列表对应元素为子列表

### 六、搜索与计数
- `find` - 查找第一个满足谓词的元素
- `any` - 判断是否存在满足谓词的元素
- `every` - 判断是否所有元素满足谓词
- `count` - 统计满足谓词的元素个数
- `list-index` - 查找第一个满足谓词的元素索引

### 七、删除与去重
- `delete` - 删除指定元素
- `delete-duplicates` - 删除列表中的重复元素

### 八、关联列表
- `alist-cons` - 在关联列表头部添加键值对
