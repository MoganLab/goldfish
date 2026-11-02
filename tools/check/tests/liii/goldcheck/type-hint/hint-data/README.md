# (liii goldcheck type-hint hint-data) 模块函数分类索引

类型提示库数据的提取与推导，包含约定查询函数名、路径映射与签名语法解析。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、名称与路径推导
- `quote-marker?` - 判定对象是否为 quote 标记符号
- `query-func-name` - 推导类型提示库的约定查询函数符号
- `expected-lib-name` - 根据文件路径推导预期的类型提示库名
- `orig-lib-path` - 根据 hint 路径推导原库路径

### 二、库定义与查询条目提取
- `hint-var-defs` - 提取所有以 hint 开头的变量定义记录
- `signature-value` - 提取变量定义对应的签名数据
- `query-define` - 查找查询函数的定义记录
- `registered-entries` - 提取查询函数中 registered 的条目列表
- `find-def` - 在定义列表中按名字查找定义记录

### 三、签名语法解析
- `type-ok?` - 判定类型指示符是否合法
- `signature-parts` - 解析签名并分解为参数类型列表与返回类型
- `signature-ok?` - 校验单条签名语法是否合法
- `signature-arity` - 获取单条签名的参数元数
