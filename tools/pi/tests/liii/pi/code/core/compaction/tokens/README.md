# (liii pi code core compaction tokens) 模块函数分类索引

`(liii pi code core compaction tokens)` 提供上下文 token 估算、压缩阈值判定与压缩配置能力。

使用时导入：

```scheme
(import (liii pi code core compaction tokens))
```

## 查看函数文档

```bash
gf doc pi-estimate-content-chars
gf doc pi-should-compact?
gf doc make-pi-compaction-settings
```

## 函数分类索引

### Token 估算
- `pi-estimate-content-chars` - 估算消息内容的字符数
- `pi-estimate-message-tokens` - 估算单条消息占用的 token 数
- `pi-context-token-estimate` - 汇总上下文 token 估算信息
- `pi-context-token-estimate?` - 判断对象是否为上下文 token 估算记录
- `pi-context-token-estimate-tokens` - 读取上下文 token 估算记录的总 token 数
- `pi-context-token-estimate-usage-tokens` - 读取上下文 token 估算记录的使用 token 数
- `pi-context-token-estimate-trailing-tokens` - 读取上下文 token 估算记录的尾部 token 数
- `make-pi-context-token-estimate` - 创建上下文 token 估算记录

### 压缩配置
- `pi-should-compact?` - 判断当前上下文 token 数是否达到压缩阈值
- `pi-compaction-default-settings` - 获取默认的压缩配置
- `make-pi-compaction-settings` - 创建压缩配置记录
- `pi-compaction-settings?` - 判断对象是否为压缩配置记录
- `pi-compaction-settings-enabled` - 读取压缩配置中的启用状态
- `pi-compaction-settings-reserve-tokens` - 读取压缩配置中的预留 token 数
- `pi-compaction-settings-keep-recent-tokens` - 读取压缩配置中的保留 token 数
