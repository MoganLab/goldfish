# (liii pi ai config record) 模块函数索引

`(liii pi ai config record)` 提供重试配置、全局设置、凭据以及模型元数据的高级构造器与 alist 转换支持。

使用时导入：

```scheme
(import (liii pi ai config record))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-settings
```

## 函数分类索引

### 重试配置
- `pi-retry-config` - 构造重试策略 record

### 客户端设置
- `pi-settings` - 构造客户端全局设置 record
- `pi-settings-model-thinking-level` - 查询特定模型的思考级别覆盖

### 凭据管理
- `pi-credential` - 构造服务商凭据 record

### 模型元数据
- `pi-model-metadata` - 构造模型元数据 record
