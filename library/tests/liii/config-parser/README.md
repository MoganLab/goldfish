# (liii config-parser) 模块函数分类索引

`(liii config-parser)` 是 Python configparser 风格的 INI 配置解析库，支持 DEFAULT 段继承、键名大小写不敏感与多种布尔字面量。

## 查看函数文档

```bash
gf doc liii/config-parser
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/config-parser "config-read-string"
```

## 函数分类索引

### 一、构造与读取
- `make-config-parser` - 创建空的配置解析器
- `config-parser?` - 判断值是否为配置解析器
- `config-read-string` - 从字符串读取配置
- `config-read-file` - 从文件读取配置

### 二、查询
- `config-sections` - 返回所有 section 名列表（不含 DEFAULT）
- `config-has-section?` - 检查 section 是否存在
- `config-has-option?` - 检查 option 是否存在
- `config-get` - 获取值（字符串）
- `config-get-int` - 获取整数值
- `config-get-boolean` - 获取布尔值
- `config-options` - 获取 section 下所有 option 名
- `config-items` - 获取 section 下所有 (key . value) 对

### 三、修改
- `config-set!` - 设置值
- `config-add-section!` - 添加 section
- `config-remove-section!` - 删除 section
- `config-remove-option!` - 删除 option

### 四、输出
- `config-write` - 将配置写出到端口
