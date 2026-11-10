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
- `pi-retry-config->alist` - 将重试策略转换为 alist

### 客户端设置
- `pi-settings` - 构造客户端全局设置 record
- `pi-settings->alist` - 将客户端设置转换为 alist
- `pi-settings-model-thinking-level` - 查询特定模型的思考级别覆盖

### 凭据管理
- `pi-credential` - 构造服务商凭据 record
- `pi-credential->alist` - 将凭据转换为 alist

### 模型元数据
- `pi-model-metadata` - 构造模型元数据 record
- `pi-model-metadata->alist` - 将模型元数据转换为 alist
