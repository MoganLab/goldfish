# (liii pi code core extensions registry base) 模块函数分类索引

`(liii pi code core extensions registry base)` 提供注册表对象解析、条目去重更新、监听器查询以及当前/默认注册表管理。

使用时导入：

```scheme
(import (liii pi code core extensions registry base))
```

## 查看函数文档

```bash
gf doc target->registry
gf doc upsert-by-key
gf doc current-extension-registry
gf doc with-extension-registry
```

## 函数分类索引

### 注册表解析
- `target->registry` - 从扩展 API、上下文或注册表记录中解析出注册表记录
- `upsert-by-key` - 按键更新列表中的条目，不存在时追加到末尾
- `extension-registry-get-listeners` - 读取注册表中指定事件类型的监听器列表

### 当前与默认注册表
- `current-extension-registry` - 读取当前生效的扩展注册表记录
- `set-current-extension-registry!` - 设置当前生效的扩展注册表记录
- `with-extension-registry` - 在临时绑定注册表的作用域内执行过程
- `default-extension-registry` - 获取进程级默认扩展注册表
- `set-default-extension-registry!` - 设置进程级默认扩展注册表记录
- `set-registry-initializer!` - 注册默认注册表的初始化过程
- `add-registry-initializer!` - 注册默认注册表的初始化过程
