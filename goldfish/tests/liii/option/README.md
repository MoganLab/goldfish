# (liii option) 模块函数分类索引

`option` 用于表示“有值”或“无值”的可选结果。它适合安全链式处理、默认值回退，以及避免直接使用 `#f` 表示缺失。

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
bin/gf doc liii/option "函数名"
```

例如：

```bash
bin/gf doc liii/option "option"
bin/gf doc liii/option "option-map"
```

## 函数分类索引

### 一、构造函数
- `none` - 创建空 option
- `option` - 创建带值 option

### 二、谓词函数
- `option?` - 判断对象是否为 option
- `option-defined?` - 判断 option 是否有值
- `option-empty?` - 判断 option 是否为空

### 三、高阶操作
- `option-map` - 对有值 option 做映射
- `option-filter` - 按谓词保留或清空 option
- `option-flat-map` - 返回新的 option 结果
- `option-for-each` - 对有值 option 执行副作用
- `option-every` - 判断值是否满足谓词
- `option-any` - 判断值是否存在真值

### 四、取值与回退
- `option-get` - 获取内部值，空 option 时报错
- `option-get-or-else` - 空 option 时返回默认值
- `option-or-else` - 空 option 时返回备用 option

### 五、比较函数
- `option=?` - 判断两个 option 的状态和值是否相等