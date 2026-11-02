# (liii tui terminal key-map) 模块函数索引

`(liii tui terminal key-map)` 提供终端控制台按键码到按键符号的映射表，覆盖标准控制字符、方向与导航键、Ctrl/Alt 组合键以及 F1-F12 功能键。

使用时导入：

```scheme
(import (liii tui terminal key-map))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc %map-standard-key
```

## 函数分类索引

### 按键码映射
- `%map-standard-key` - 映射标准控制台字节码为按键
- `%map-extended-key` - 映射控制台扩展键码为按键
