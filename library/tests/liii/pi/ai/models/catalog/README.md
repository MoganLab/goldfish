# (liii pi ai models catalog) 模块函数索引

`(liii pi ai models catalog)` 提供模型目录管理与聚合工具，支持从目录中获取指定服务商模型、提取去重的服务商名称列表，以及按模型 ID 覆盖合并。

使用时导入：

```scheme
(import (liii pi ai models catalog))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-catalog-provider-names
```

## 函数分类索引

### 目录查询与合并
- `pi-catalog-provider-names` - 获取模型目录中所有服务商名称
- `pi-catalog-provider-models` - 获取指定服务商包含的模型列表
- `pi-catalog-merge-provider-models` - 按模型 ID 覆盖合并模型列表
