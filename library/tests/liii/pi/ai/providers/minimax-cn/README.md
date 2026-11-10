# (liii pi ai providers minimax-cn) 模块函数索引

`(liii pi ai providers minimax-cn)` 提供 MiniMax 国内版服务商的内置配置定义、模型元数据及额度耗尽错误识别。

使用时导入：

```scheme
(import (liii pi ai providers minimax-cn))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc PI_MINIMAX_CN_QUOTA_ERROR
```

## 函数分类索引

### 服务商配置与模型定义
- `PI_MINIMAX_CN_PROVIDER` - MiniMax 国内版服务商基础配置
- `PI_MINIMAX_CN_BUILTIN_MODELS` - MiniMax 国内版内置模型定义列表

### 额度错误识别
- `PI_MINIMAX_CN_QUOTA_ERROR` - 识别 MiniMax 国内版额度耗尽错误
