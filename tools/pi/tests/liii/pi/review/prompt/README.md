# (liii pi review prompt) 模块函数分类索引

`(liii pi review prompt)` 提供代码审查提示词构建能力，包含结构化 JSON 提示词与非结构化降级提示词。

使用时导入：

```scheme
(import (liii pi review prompt))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-build-review-prompt
gf doc pi-build-fallback-prompt
```

## 函数分类索引

### 提示词构建
- `pi-build-review-prompt` - 构建结构化代码审查提示词，包含 JSON 契约与语言规则清单
- `pi-build-fallback-prompt` - 构建非结构化降级审查提示词
