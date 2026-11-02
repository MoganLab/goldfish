# (liii goldcheck path) 模块函数分类索引

代码规范检查中的路径处理与 glob 匹配工具集，提供跨平台路径归一化、排除规则判定与相对路径转换等能力。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、路径归一化与转换
- `normalize-sep` - 将路径分隔符统一替换为正斜杠并去除 `./` 前缀
- `relativize-path` - 将绝对路径转换为相对于仓库根目录的相对路径

### 二、字符串与模式匹配
- `has-wildcard?` - 判断模式字符串中是否包含通配符 `*`
- `string-index-of` - 在字符串中查找子串首次出现的索引位置
- `string-starts-with?` - 判断字符串是否以指定前缀开头
- `glob-match?` - 通配符模式匹配判定

### 三、排除项规则匹配
- `exclude-entry->path` - 将配置排除项统一转换为路径模式字符串
- `path-matches-exclude?` - 判断相对路径是否命中单条排除模式
- `file-excluded?` - 判断文件路径是否匹配排除规则列表中的任意一项
