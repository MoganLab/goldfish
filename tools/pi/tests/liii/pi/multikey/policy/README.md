# (liii pi multikey policy) 模块函数分类索引

`(liii pi multikey policy)` 提供密钥池轮换策略：cooldown 与固定当前 key 的状态机，并经 config 钩子接入核心请求路径。

使用时导入：

```scheme
(import (liii pi multikey policy))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-multikey-select-key
gf doc pi-multikey-mark-cooldown!
gf doc pi-multikey-pool-of
gf doc pi-multikey-request-key
gf doc pi-multikey-rotate!
```

## 函数分类索引

### 密钥选择与冷却
- `pi-multikey-select-key` - 从密钥池中选取当前可用的密钥
- `pi-multikey-mark-cooldown!` - 按失败原因标记密钥冷却时长

### 请求接入
- `pi-multikey-pool-of` - 从会话配置中读取绑定的密钥池记录
- `pi-multikey-request-key` - 为本次请求选取应使用的密钥
- `pi-multikey-rotate!` - 冷却当前密钥并切换到下一个可用密钥
