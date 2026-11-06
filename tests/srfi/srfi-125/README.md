# (srfi srfi-125) 模块函数分类索引

SRFI-125 是可变中间哈希表（Intermediate Hash Tables）规范，提供了构造、访问、更新、遍历及比较等操作。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc srfi/srfi-125 "hash-table-ref"
```
或者直接按函数名检索：
```bash
gf doc "hash-table-ref"
```

## 函数分类索引

### 一、构造器
- `make-hash-table` - 创建空哈希表
- `hash-table` - 从键值对直接构造哈希表
- `alist->hash-table` - 从关联列表构造哈希表
- `hash-table-copy` - 复制哈希表

### 二、谓词
- `hash-table?` - 判断是否为哈希表
- `hash-table-contains?` - 判断是否包含指定键
- `hash-table-empty?` - 判断哈希表是否为空
- `hash-table=?` - 判断两个哈希表是否相等

### 三、访问与更新
- `hash-table-ref` - 读取键对应的值
- `hash-table-ref/default` - 读取键对应的值，未命中时返回默认值
- `hash-table-set!` - 设置键值对
- `hash-table-delete!` - 删除指定键
- `hash-table-update!` - 使用过程更新键值
- `hash-table-update!/default` - 使用过程更新键值，未命中时使用默认值
- `hash-table-clear!` - 清空哈希表

### 四、大小与键值集合
- `hash-table-size` - 获取键值对数量
- `hash-table-keys` - 获取所有键的列表
- `hash-table-values` - 获取所有值的列表
- `hash-table-entries` - 获取键与值的多返回值
- `hash-table->alist` - 转换为关联列表

### 五、遍历与折叠
- `hash-table-find` - 查找满足条件的键值
- `hash-table-count` - 统计满足条件的键值对数
- `hash-table-fold` - 折叠遍历所有键值对
- `hash-table-for-each` - 遍历执行副作用
- `hash-table-map->list` - 映射键值并生成列表
