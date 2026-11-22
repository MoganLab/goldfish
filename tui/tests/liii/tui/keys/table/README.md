# (liii tui keys table) 模块函数索引

`(liii tui keys table)` 提供终端按键码表的底层查询函数，将
控制字节、转义序列的终结字节与数字参数转换为按键名或修饰键
前缀，供 `(liii tui keys parser)` 解析按键流时使用。

使用时导入：

```scheme
(import (liii tui keys table))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc %ctrl-char-key
```

## 函数分类索引

### 控制字节
- `%ctrl-char-key` - 将 ASCII 控制字节转换为控制键名

### 转义序列
- `%ss3-key` - 将 SS3 (ESC O X) 序列的终结字节转换为按键名
- `%csi-final-key` - 将 CSI 序列的终结字节转换为按键名
- `%tilde-key` - 将 CSI ~ 序列的数字参数转换为按键名
- `%linux-fn-key` - 将 Linux 控制台功能键字节转换为功能键名

### 修饰键
- `%modifier-prefix` - 将修饰键代码转换为键名前缀字符串
- `%alt-wrap` - 把 Meta 前缀与基础键名合成为 alt- 组合键名

### 双 ESC Meta 序列判定
- `%csi-final-byte?` - 判断字节是否为 CSI 序列终结字节（0x40~0x7E）
- `%csi-meta-prefix?` - 判断字节是否为双 ESC Meta 序列中允许的序列起始字节（[ 或 O）
- `%meta-escape-prefix?` - 判断字节流是否为双 ESC Meta 序列前缀（macOS Option=Esc+）
- `%meta-escape-complete?` - 判断双 ESC Meta 序列字节是否已聚齐
