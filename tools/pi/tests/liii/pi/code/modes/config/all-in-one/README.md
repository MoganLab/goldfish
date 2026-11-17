# (liii pi code modes config all-in-one) 模块函数分类索引

`(liii pi code modes config all-in-one)` 为 `gf pi config` 的 `all-in-one` 子标签页提供设置链条数据装配、详情生成与只读文件视图。

## 使用方法

```scheme
(import (liii pi code modes config all-in-one))
```

## 函数列表

- `pi-config-aio-state` - 装配 all-in-one 子标签页的状态视图
- `pi-config-aio-chains` - 读取状态中的链条列表
- `pi-config-aio-chain-nodes` - 读取链条中的有序节点列表
- `pi-config-aio-chain-detail-rows` - 生成链条右列详情行
- `pi-config-aio-node-detail-rows` - 生成节点右列详情行
- `pi-config-aio-file-view` - 生成 all-in-one.json 的只读覆盖层视图
- `pi-config-aio-status-text` - 组装 all-in-one 子标签页状态栏文本
- `pi-config-aio-enabled-targets` - 获取所有已启用的非 all-in-one 供应商及其已启用模型
