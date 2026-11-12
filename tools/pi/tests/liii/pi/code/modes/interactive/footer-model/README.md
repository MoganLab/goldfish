# (liii pi code modes interactive footer-model) 模块函数分类索引

`(liii pi code modes interactive footer-model)` 负责页脚右下角模型标签的纯函数组装：
普通供应商显示 `(provider) model • level`；all-in-one 伪模型精简为
`all-in-one/<chain-id>`，并把当前实际节点追加为 `-> (provider) model • level`。

```scheme
(import (liii pi code modes interactive footer-model))
```

## 函数分类索引

- `pi-footer-model-label` - 组装单个模型标签串
- `pi-footer-model-part` - 组装右侧完整模型串（含 all-in-one 精简与节点后缀）
