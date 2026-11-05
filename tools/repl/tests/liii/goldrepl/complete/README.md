# (liii goldrepl complete) 模块导览

`(liii goldrepl complete)` 提供 REPL 的符号补全能力：维护全局符号缓存、按前缀筛选补全候选、
计算光标处的当前词并就地替换，同时提供候选词之间的最长公共前缀计算。

使用时导入（load-path 需包含 `tools/repl`）：

```scheme
(import (liii goldrepl complete))
```

## 查看函数文档

```bash
# 查看模块导览（load-path 需包含 tools/repl）
gf doc liii/goldrepl/complete

# 查看具体函数的文档
gf doc liii/goldrepl/complete "%repl-completions"
```

## 函数分类索引

### 符号缓存

- `*repl-symbols*` - 全局符号缓存，保存 `(符号名 . 文档)` 形式的条目列表。
- `%repl-update-symbol-cache!` - 扫描根环境，重建全局符号缓存。
- `%repl-invalidate-symbol-cache!` - 将符号缓存标记为失效，等待下次访问时惰性重建。

### 补全候选

- `%repl-completions` - 按前缀筛选符号缓存中的补全候选。
- `%repl-symbol-name?` - 判断符号名是否已收录在符号缓存中。
- `%repl-symbol-char?` - 判断字符是否可以出现在符号名中。

### 光标词处理

- `%repl-word-at` - 取出光标左侧的当前词。
- `%repl-replace-word` - 替换光标左侧的当前词，并保留其余文本。

### 公共前缀

- `%repl-string-common-prefix` - 计算两个字符串的最长公共前缀。
- `%repl-common-prefix` - 计算字符串列表的最长公共前缀。
