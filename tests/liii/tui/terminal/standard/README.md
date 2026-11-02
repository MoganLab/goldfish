# (liii tui terminal standard) 模块函数索引

`(liii tui terminal standard)` 提供基于 ANSI 转义序列的标准（真实）终端实现，支持输出端口重定向、动态尺寸探测以及 inline 模式下的相对光标定位。

使用时导入：

```scheme
(import (liii tui terminal standard))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-standard-terminal
```

## 函数分类索引

### 标准终端创建与谓词
- `make-standard-terminal` - 创建标准终端实例
- `standard-terminal?` - 判断是否为标准终端实例
