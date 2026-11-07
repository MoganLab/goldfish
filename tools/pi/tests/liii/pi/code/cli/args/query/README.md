# (liii pi code cli args query) 模块函数分类索引

`(liii pi code cli args query)` 提供解析后参数字典的读取、写入与超时解析能力。

使用时导入：

```scheme
(import (liii pi code cli args query))
```

## 查看函数文档

```bash
gf doc pi-args-ref
gf doc pi-args-set
gf doc pi-resolve-timeout
```

## 函数分类索引

### 参数读写
- `pi-args-ref` - 从参数字典中读取指定键的取值
- `pi-args-set` - 向参数字典写入或覆盖指定键的取值

### 超时解析
- `pi-resolve-timeout` - 解析请求超时秒数
