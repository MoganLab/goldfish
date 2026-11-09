# (srfi srfi-117) 模块函数分类索引

SRFI-117 是基于列表的可变双端队列（Mutable Queues based on lists）参考实现，
同时被 `(liii queue)` 完整重导出，可用于 FIFO 或双端访问的场景。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-117 "list-queue-front"
```

## 函数分类索引

### 一、构造函数
- `make-list-queue` - 从列表创建队列（支持单参数和双参数形式）
- `list-queue` - 从任意参数创建队列
- `list-queue-copy` - 复制队列
- `list-queue-unfold` - 使用 unfold 创建队列
- `list-queue-unfold-right` - 使用 unfold-right 创建队列

### 二、谓词
- `list-queue?` - 检查是否为队列
- `list-queue-empty?` - 检查队列是否为空

### 三、访问器
- `list-queue-front` - 获取队列前端元素
- `list-queue-back` - 获取队列后端元素
- `list-queue-list` - 获取队列的列表表示
- `list-queue-first-last` - 获取首尾点对（多返回值）

### 四、修改器
- `list-queue-add-front!` - 在前端添加元素
- `list-queue-add-back!` - 在后端添加元素
- `list-queue-remove-front!` - 从前端移除并返回元素
- `list-queue-remove-back!` - 从后端移除并返回元素
- `list-queue-remove-all!` - 移除所有元素，返回列表
- `list-queue-set-list!` - 设置队列为新列表

### 五、连接操作
- `list-queue-append` - 连接多个队列，返回新队列
- `list-queue-append!` - 连接多个队列，修改第一个队列
- `list-queue-concatenate` - 连接队列列表

### 六、映射与遍历
- `list-queue-map` - 映射函数到队列元素，返回新队列
- `list-queue-map!` - 映射函数到队列元素，修改原队列
- `list-queue-for-each` - 遍历队列元素
