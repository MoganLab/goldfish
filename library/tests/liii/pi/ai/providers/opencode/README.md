# (liii pi ai providers opencode) 模块函数索引

`(liii pi ai providers opencode)` 提供 OpenCode Go 编程订阅服务商的内置配置定义、模型元数据及额度耗尽错误识别。

使用时导入：

```scheme
(import (liii pi ai providers opencode))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc PI_OPENCODE_QUOTA_ERROR
```

## 函数分类索引

### 服务商配置与模型定义
- `PI_OPENCODE_PROVIDER` - OpenCode 服务商基础配置
- `PI_OPENCODE_BUILTIN_MODELS` - OpenCode 内置模型定义列表

### 额度错误识别
- `PI_OPENCODE_QUOTA_ERROR` - 识别 OpenCode Go 订阅额度耗尽错误
