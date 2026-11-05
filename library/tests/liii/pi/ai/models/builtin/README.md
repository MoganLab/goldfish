# (liii pi ai models builtin) 模块函数索引

`(liii pi ai models builtin)` 提供内置模型元数据在模型存储中的补齐与增强机制，支持单服务商补齐及全量内置服务商模型合并。

使用时导入：

```scheme
(import (liii pi ai models builtin))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-builtin-augment-all
```

## 函数分类索引

### 模型补齐与增强
- `pi-builtin-augment-store` - 将指定服务商的内置模型补齐至模型存储
- `pi-builtin-augment-all` - 将全部内置服务商的模型补齐至模型存储
