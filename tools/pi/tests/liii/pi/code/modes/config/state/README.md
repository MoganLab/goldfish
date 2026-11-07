# (liii pi code modes config state) 模块函数分类索引

`(liii pi code modes config state)` 提供 `gf pi config` 只读视图模型：把用户配置目录装配成 ranger 三列所需的数据。

使用时导入：

```scheme
(import (liii pi code modes config state))
```

## 查看函数文档

```bash
gf doc pi-config-state
gf doc pi-config-provider
gf doc pi-config-model-detail-rows
```

## 函数分类索引

### 配置视图
- `pi-config-state` - 把用户配置目录装配成只读视图模型
- `pi-config-state?` - 判断对象是否为配置视图记录
- `pi-config-state-dir` - 读取配置视图记录对应的配置目录
- `pi-config-state-providers` - 读取配置视图记录中的 provider 列表
- `pi-config-state-default-provider` - 读取配置视图记录中的默认 provider
- `pi-config-state-default-model` - 读取配置视图记录中的默认模型
- `pi-config-state-provider-count` - 统计配置视图记录中的 provider 数量
- `pi-config-state-model-count` - 统计配置视图记录中的模型总数
- `pi-config-state-enabled-provider-count` - 统计配置视图中可用的 provider 数量
- `pi-config-state-enabled-model-count` - 统计配置视图中可用的模型数量

### Provider 条目
- `pi-config-provider` - 按标识查找配置视图中的 provider 条目
- `pi-config-provider-id` - 读取 provider 条目的标识
- `pi-config-provider-name` - 读取 provider 条目的显示名称
- `pi-config-provider-base-url` - 读取 provider 条目的基础 URL
- `pi-config-provider-api` - 读取 provider 条目的 API 协议类型
- `pi-config-provider-proxy` - 读取 provider 条目的生效代理地址
- `pi-config-provider-key-source` - 读取 provider 条目的密钥来源标记
- `pi-config-provider-models` - 读取 provider 条目下的模型列表
- `pi-config-provider-default?` - 判断 provider 是否为默认 provider
- `pi-config-provider-file` - 读取 provider 的本地配置文件相对路径
- `pi-config-provider-enabled?` - 判断 provider 是否可用

### 模型条目
- `pi-config-model-id` - 读取模型条目的标识
- `pi-config-model-name` - 读取模型条目的显示名称
- `pi-config-model-context` - 读取模型条目的上下文窗口大小
- `pi-config-model-max-tokens` - 读取模型条目的最大输出 token 数
- `pi-config-model-reasoning?` - 判断模型是否支持推理模式
- `pi-config-model-file` - 读取模型的本地配置文件相对路径
- `pi-config-model-default?` - 判断模型是否为默认模型
- `pi-config-model-enabled?` - 判断模型是否可用
- `pi-config-model-detail-rows` - 生成模型详情行的标签与取值列表
