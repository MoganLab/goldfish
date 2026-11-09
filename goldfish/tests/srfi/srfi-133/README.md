# (srfi srfi-133) 模块函数分类索引

SRFI-133 是向量处理库（Vector Library），提供了全面的向量展开、搜索、折叠、反转与变更等操作。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc srfi/srfi-133 "vector-fold"
```
或者直接按函数名检索：
```bash
gf doc "vector-fold"
```

## 函数分类索引

### 一、构造器与展开
- `vector-unfold` - 使用过程展开构造向量
- `vector-unfold-right` - 从右向左展开构造向量
- `vector-unfold!` - 原地展开填充向量
- `vector-unfold-right!` - 从右向左原地展开填充向量
- `vector-reverse-copy` - 反向复制向量
- `vector-reverse-copy!` - 原地反向复制向量片段
- `vector-concatenate` - 连接向量列表
- `vector-append-subvectors` - 拼接子向量区间

### 二、谓词与比较
- `vector-empty?` - 判断向量是否为空
- `vector=` - 使用元素等价关系比较向量

### 三、搜索与查找
- `vector-index` - 查找首个满足谓词的元素索引
- `vector-index-right` - 从右向左查找首个满足谓词的元素索引
- `vector-skip` - 跳过满足谓词的首个元素索引
- `vector-skip-right` - 从右向左跳过满足谓词的首个元素索引
- `vector-binary-search` - 二分查找元素索引
- `vector-any` - 检查是否存在满足谓词的元素
- `vector-every` - 检查是否所有元素均满足谓词

### 四、修改器
- `vector-swap!` - 交换两个索引处的元素
- `vector-reverse!` - 原地反转向量元素
- `vector-map!` - 原地映射更新向量元素

### 五、折叠与统计
- `vector-fold` - 从左向右折叠向量
- `vector-fold-right` - 从右向左折叠向量
- `vector-count` - 统计满足谓词的元素数量
- `vector-cumulate` - 前缀累计前缀向量
- `vector-partition` - 依据谓词分割向量

### 六、转换
- `reverse-vector->list` - 反向将向量转换为列表
- `reverse-list->vector` - 反向将列表转换为向量
