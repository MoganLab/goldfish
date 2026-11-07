# (liii pi code core extensions event) 模块函数分类索引

`(liii pi code core extensions event)` 提供扩展事件监听器注册与事件派发能力，支持监听器阻断、终止与输入覆盖。

使用时导入：

```scheme
(import (liii pi code core extensions event))
```

## 查看函数文档

```bash
gf doc pi-on!
gf doc pi-emit!
```

## 函数分类索引

### 事件生命周期
- `pi-on!` - 为指定事件类型注册监听器
- `pi-emit!` - 向扩展监听器派发生命周期事件
