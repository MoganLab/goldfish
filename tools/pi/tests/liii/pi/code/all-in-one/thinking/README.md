# (liii pi code all-in-one thinking) 模块函数分类索引

`(liii pi code all-in-one thinking)` 负责把用户显式等级、节点默认等级与真实物理模型能力收敛成最终生效的 Thinking Level，并提供收敛诊断日志。

```scheme
(import (liii pi code all-in-one thinking))
```

## 查看函数文档

```bash
gf doc resolve-node-thinking-level
gf doc pi-log-thinking-clamp!
```

## 函数分类索引

### 收敛

- `resolve-node-thinking-level` - 计算节点最终生效的 Thinking Level

### 收敛诊断

- `pi-log-thinking-clamp!` - 显式等级被节点能力收敛时写 `[THINKING_CLAMP]` 日志

## 收敛不变量

优先级：用户显式等级 > 节点 `all-in-one.json` 的 `thinkingLevel` > `off`。

随后必须按真实物理模型能力收敛：模型不支持思考时收敛为 `off`；否则按可用等级集合
就近收敛（先向高找、再向低找）。探活与业务请求共用同一收敛结果，避免探活通过而
正式请求返回 HTTP 400。

显式值被改变时写一条 `[THINKING_CLAMP]` 日志；没有 `emit` 回调时同时在 stderr
输出一行提示，使脚本模式也能看到收敛结果。
