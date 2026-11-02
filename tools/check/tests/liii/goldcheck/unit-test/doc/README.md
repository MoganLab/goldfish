# (liii goldcheck unit-test doc) 模块函数分类索引

单元测试头部文档注释解析与合规性检查，确保单元测试文档包含标准小节（语法、参数、返回值等）且格式规范。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、文档解析与校验
- `%standard-sections` - 允许的标准小节标题列表
- `divider-string?` - 判断字符串是否为分割线（如 '----'）
- `comment-body` - 提取行注释的分号后有效正文
- `extract-doc-entries` - 从行列表中提取顶部连续注释条目
- `check-doc-elements` - 校验文档注释结构是否包含必须的小节
