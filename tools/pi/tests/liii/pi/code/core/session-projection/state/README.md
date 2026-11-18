# (liii pi code core session-projection state) 模块函数分类索引

`(liii pi code core session-projection state)` 提供会话投影状态的汇总能力。

使用时导入：

```scheme
(import (liii pi code core session-projection state))
```

## 查看函数文档

```bash
gf doc pi-collect-projection-state
```

## 函数分类索引

### 状态汇总
- `pi-collect-projection-state` - 汇总会话投影中的思考级别与模型来源状态（模型来源带 `modelSource` 标记：`model_change` 为用户显式切换，`assistant` 为上一轮实际服务的物理节点）
