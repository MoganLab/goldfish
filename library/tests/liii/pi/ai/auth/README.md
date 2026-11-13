# (liii pi ai auth) 模块函数索引

`(liii pi ai auth)` 提供各 AI 服务商 API 密钥与凭据的加载和解析，支持读取 `auth.json`、多级环境变量覆盖（如 `GEMINI_API_KEY`、`DEEPSEEK_API_KEY`）以及服务商别名回退。

使用时导入：

```scheme
(import (liii pi ai auth))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-resolve-provider-key
```

## 函数分类索引

### 凭据加载与解析
- `pi-load-auth` - 从 auth.json 文件加载凭据列表
- `pi-resolve-provider-key` - 解析指定服务商的生效 API 密钥
- `pi-save-auth-key` - 向 auth.json 中保存或增量更新服务商 API 密钥凭证
