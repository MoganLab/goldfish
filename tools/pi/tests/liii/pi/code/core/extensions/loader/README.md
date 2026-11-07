# (liii pi code core extensions loader) 模块函数分类索引

`(liii pi code core extensions loader)` 提供扩展脚本的加载与加载结果汇总能力。

使用时导入：

```scheme
(import (liii pi code core extensions loader))
```

## 查看函数文档

```bash
gf doc pi-load-extension
gf doc pi-load-extensions
gf doc pi-load-results-success-count
gf doc pi-load-results-failed-count
```

## 函数分类索引

### 扩展加载
- `pi-load-extension` - 加载单个扩展脚本并返回加载结果
- `pi-load-extensions` - 批量加载扩展脚本并汇总加载结果
- `pi-load-results-success-count` - 读取扩展加载报告中的成功数量
- `pi-load-results-failed-count` - 读取扩展加载报告中的失败数量
