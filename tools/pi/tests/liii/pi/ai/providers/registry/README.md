# (liii pi ai providers registry) 模块函数索引

`(liii pi ai providers registry)` 集中管理所有支持的 AI 服务商（Provider）注册表，包括端点 baseUrl、协议类型（api）、API Key 环境变量及内置模型目录。

使用时导入：

```scheme
(import (liii pi ai providers registry))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-provider-record
```

## 函数分类索引

### 注册表与常量定义
- `PI_PROVIDERS` - 全部内置服务商配置注册表
- `PI_SUPPORTED_PROVIDERS` - 支持的服务商名称列表
- `PI_PROVIDER_BUILTIN_MODELS` - 内置模型目录关联表

### 服务商配置查询
- `pi-provider-record` - 获取服务商完整配置
- `pi-provider-api` - 获取服务商请求协议标识
- `pi-provider-base-url` - 获取服务商默认端点地址
- `pi-provider-env-keys` - 获取服务商 API Key 环境变量列表
- `pi-provider-builtin-models` - 获取服务商内置模型列表

### 额度错误处理
- `pi-provider-quota-error-handler` - 获取服务商额度错误处理函数
- `pi-provider-quota-error` - 调用服务商额度错误识别
