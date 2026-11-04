# (liii json) 模块导览

`(liii json)` 提供 JSON 文本的解析与序列化，以及 Scheme-JSON 数据结构的
访问、更新与类型判断。Scheme-JSON 用关联列表表示对象、vector 表示数组，
`'()` 表示 JSON null。

使用时导入：

```scheme
(import (liii json))
```

JSON Schema 校验功能由 `(liii json schema)` 子模块提供，详见
`tests/liii/json/schema/`。

## 查看函数文档

使用以下命令查看函数的文档和用例：

```bash
gf doc liii/json "string->json"
gf doc liii/json "json-ref"
```

## 函数分类索引

### 一、解析与序列化

JSON 文本与 Scheme-JSON 数据结构之间的转换：

| 函数 | 说明 |
|---|---|
| `string->json` | 解析 JSON 字符串为 Scheme-JSON 数据结构 |
| `json->string` | 将 Scheme-JSON 数据结构编码为字符串 |
| `read-json` | 从输入端口流式读取下一个 JSON 实体 |
| `write-json` | 将数据结构序列化写入输出端口 |
| `json-string-escape` | 转义字符串中的特殊字符 |

### 二、访问与更新

读取、修改与变换 Scheme-JSON 数据结构：

| 函数 | 说明 |
|---|---|
| `json-ref` | 按键读取值，支持嵌套路径 |
| `json-get-or-else` | 按键读取值，不存在时返回默认值 |
| `json-set` | 设置键值，支持嵌套路径 |
| `json-push` | 向数组尾部追加元素 |
| `json-drop` | 删除指定键 |
| `json-reduce` | 按键路径或谓词变换值 |
| `json-keys` | 获取对象当前层级的所有键 |
| `json-contains-key?` | 判断对象是否包含指定键 |

### 三、类型判断

判断值的 JSON 类型：

| 函数 | 说明 |
|---|---|
| `json-null?` | 判断是否为 JSON null |
| `json-object?` | 判断是否为 JSON 对象 |
| `json-array?` | 判断是否为 JSON 数组 |
| `json-string?` | 判断是否为 JSON 字符串 |
| `json-number?` | 判断是否为 JSON 数值 |
| `json-integer?` | 判断是否为 JSON 整数 |
| `json-float?` | 判断是否为 JSON 浮点数 |
| `json-boolean?` | 判断是否为 JSON 布尔值 |

### 四、类型安全获取器

按键读取并做类型检查，类型不符或键不存在时返回默认值：

| 函数 | 说明 |
|---|---|
| `json-ref-string` | 安全读取字符串值 |
| `json-ref-number` | 安全读取数值 |
| `json-ref-integer` | 安全读取整数 |
| `json-ref-boolean` | 安全读取布尔值 |

## 回归测试

| 文件 | 说明 |
|---|---|
| `e2e-string-to-json-memory-leak-test.scm` | issue 1311：大输入循环解析的 RSS 增长回归 |
