# (liii pi review language) 模块函数分类索引

`(liii pi review language)` 提供根据文件后缀识别编程语言并生成语言专属代码审查检查项的能力。

使用时导入：

```scheme
(import (liii pi review language))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-detect-language
gf doc pi-language-checklist
gf doc pi-build-language-section
```

## 函数分类索引

### 语言检测与清单生成
- `pi-detect-language` - 根据文件路径扩展名识别编程语言
- `pi-language-checklist` - 获取指定编程语言的代码审查关注点清单
- `pi-build-language-section` - 汇总指定语言集合的代码审查清单段落文本
