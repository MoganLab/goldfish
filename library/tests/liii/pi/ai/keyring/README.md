# (liii pi ai keyring) 模块函数索引

`(liii pi ai keyring)` 提供通用的 API Key 钩子接口，支持请求前动态选取 API Key（如多 key 轮询、负载均衡）以及在收到 429/403 限流响应时自动触发密钥轮换。

使用时导入：

```scheme
(import (liii pi ai keyring))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-request-api-key
```

## 函数分类索引

### 密钥获取与轮换
- `pi-request-api-key` - 获取本次请求使用的 API 密钥
- `pi-key-rotation?` - 检查配置是否支持自动换 key
- `pi-rotate-api-key!` - 触发密钥轮换钩子获取新密钥
