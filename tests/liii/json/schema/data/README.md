# (liii json schema data) 模块函数分类索引

`(liii json schema data)` 封装 JSON 数据的底层访问约定：
对象以键值对 alist 表示，数组以向量表示，
布尔值兼容 `#t`/`#f` 与 `true`/`false` 两种写法。

这些函数是 `(liii json schema)` 及其它子模块的公共基础。

## 常见用法示例

```scheme
(import (liii json schema data))

(object-ref '(("name" . "Liii")) "name" #f)   ; => "Liii"
(object-has? '(("name" . "Liii")) 'name)       ; => #t
(listify #(1 2 3))                             ; => (1 2 3)
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/data "函数名"
```

## 函数分类索引

### 一、布尔与键
- `json-boolean?` - 判断是否为 JSON 布尔值
- `json-true?` - 判断是否为 JSON 布尔真值
- `json-false?` - 判断是否为 JSON 布尔假值
- `key->string` - 把对象的键转换成字符串

### 二、对象访问
- `entries` - 取得对象的键值对序列
- `object-cell` - 查找对象的键值对单元
- `object-ref` - 读取对象中键对应的值
- `object-has?` - 判断对象中是否存在某个键

### 三、选项与遍历
- `option-ref` - 读取选项表中的值
- `listify` - 把值规约为列表
- `absent` - 缺失值标记
- `collect-for-each` - 遍历集合并收集结果

## 错误处理约定

- `key->string` 遇到既不是字符串也不是符号的键时抛出类型错误
- `absent` 用于区分「键不存在」与「键存在但值为 #f」
- `option-ref` 在键不存在时返回默认值
