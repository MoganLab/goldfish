# (liii pi review parse) 模块函数分类索引

`(liii pi review parse)` 提供模型输出的审查问题 JSON 解析与严重级常量定义。

使用时导入：

```scheme
(import (liii pi review parse))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc PI_REVIEW_SEVERITIES
gf doc pi-parse-review-findings
```

## 函数分类索引

### 常量
- `PI_REVIEW_SEVERITIES` - 代码审查支持的问题严重级别列表

### 结果解析
- `pi-parse-review-findings` - 解析模型返回的结构化代码审查问题 JSON 文本
