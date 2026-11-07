# (liii pi multikey pool) 模块函数分类索引

`(liii pi multikey pool)` 提供密钥池数据层：池与密钥的记录类型及脱敏工具。

使用时导入：

```scheme
(import (liii pi multikey pool))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-mask-key
gf doc pi-pool-key
gf doc pi-key-pool
```

## 函数分类索引

### 密钥记录
- `pi-pool-key` - 创建密钥池中的单个密钥记录
- `pi-pool-key?` - 判断对象是否为密钥池中的密钥记录
- `pi-pool-key-key` - 读取密钥记录中的密钥字符串
- `pi-pool-key-label` - 读取密钥记录中的标签
- `pi-pool-key-enabled` - 读取密钥记录的启用状态
- `pi-pool-key-last-used` - 读取密钥记录最近一次被使用的时间戳
- `pi-pool-key-set-last-used!` - 更新密钥记录最近一次被使用的时间戳
- `pi-pool-key-cooldown-until` - 读取密钥记录的冷却截止时间戳
- `pi-pool-key-set-cooldown-until!` - 更新密钥记录的冷却截止时间戳
- `pi-pool-key-cooling?` - 判断密钥在指定时刻是否仍处于冷却期

### 密钥池
- `pi-key-pool` - 创建密钥池记录
- `pi-key-pool?` - 判断对象是否为密钥池记录
- `pi-key-pool-id` - 读取密钥池的标识
- `pi-key-pool-cooldown-ms` - 读取密钥池的普通限流冷却时长
- `pi-key-pool-invalid-cooldown-ms` - 读取密钥池的密钥失效冷却时长
- `pi-key-pool-keys` - 读取密钥池中的密钥记录列表

### 脱敏
- `pi-mask-key` - 对日志与错误摘要中的密钥做脱敏处理
