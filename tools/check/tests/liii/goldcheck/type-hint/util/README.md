# (liii goldcheck type-hint util) 模块函数分类索引

type-hint 规则的基础辅助工具函数，包含字符串拼接、路径分割、符号列表合并、列表去重与排序。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、字符串与路径处理
- `join-strings` - 使用指定分隔符拼接字符串列表
- `split-slash` - 依斜杠 '/' 分割字符串为列表
- `strip-scm` - 去除路径末尾的 '.scm' 扩展名
- `syms-join` - 将符号列表转为字符串并以空格连接

### 二、列表处理
- `dedup` - 列表元素去重（保留首次出现的相对顺序）
- `sort-numbers` - 数字列表升序排序
