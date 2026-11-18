# (liii pi code all-in-one request) 模块函数分类索引

`(liii pi code all-in-one request)` 是 Agent 主循环、压缩摘要与 all-in-one 调度器之间的请求门面：按伪模型 ID 找链，组装真实请求执行器与探活器，再调用链式调度；全链不可用时接入 recovery。

```scheme
(import (liii pi code all-in-one request))
```

## 查看函数文档

```bash
gf doc pi-all-in-one-request-stream
```

## 函数分类索引

- `pi-all-in-one-request-stream` - 将元供应商请求转换为具体节点请求

## 请求门面的职责

门面只做五步，其余职责都在平铺模块里：

1. 加载用户配置，没读到就用默认配置（`all-in-one-load-or-default-config`）；
2. 解析伪模型 ID 对应的链，并兼容历史别名
   （`coding-deep` / `coding-high-level` → `coding-high-quality`，`coding-fast-cheap` → `coding-fast`）；
3. 组装真实请求执行器（节点凭据、协议、模型元数据、思考等级收敛）与探活器；
4. 调用链式调度执行一整轮，真实请求失败即记录故障并顺延；
5. 全链枯竭时按 `continuous` 决定是否进入 recovery 等待与到期重试。

`active_node` 事件的时机与载荷（provider、model、thinkingLevel、contextWindow、
supports-thinking）保持不变；`supports-thinking` 取节点真实模型能力，不取伪模型的值。

## 导出边界

只公开 `pi-all-in-one-request-stream`。全链枯竭的错误文案构造器属于内部实现，
其 `stopReason=error` 与错误说明文案由 `pi-all-in-one-request-stream-test.scm` 断言覆盖。
