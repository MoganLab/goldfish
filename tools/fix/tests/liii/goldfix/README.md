# (liii goldfix) 模块函数索引

`(liii goldfix)` 是 gf fix 工具的入口库：对外提供安全包装（repair-source / repair-source\*）与命令行入口（main），并汇聚 record、repair、cache、cli、batch 等子库。

使用时导入：

```scheme
(import (liii goldfix))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc "函数名"
```

或者指定库名：

```bash
gf doc liii/goldfix "函数名"
```

## 函数分类索引

### 命令行入口
- `main` - 解析命令行参数并执行括号修正，可显式传入参数列表供单元测试驱动

### 安全包装
- `repair-source` - 修正源码中的括号，只返回修正后的源码
- `repair-source*` - 修正源码中的括号，返回 (修正后源码 是否跳过)

## 子库导览

| 子库 | 说明 |
|---|---|
| `(liii goldfix record)` | token / 行 / 左括号环境 / 待确认右括号 / edit / report 记录类型 |
| `(liii goldfix scan)` | 词法扫描：字符分类与各类记号的边界查找 |
| `(liii goldfix tokenize)` | 分词与按行组织 token |
| `(liii goldfix edit)` | 把 edit 作用到源码文本上 |
| `(liii goldfix repair)` | 按缩进推断括号结构并生成修正结果 |
| `(liii goldfix cache)` | 缓存条目路径计算与读写 |
| `(liii goldfix cli)` | 命令行帮助、参数解析器与后缀名解析 |
| `(liii goldfix batch)` | 单文件、文件列表与目录的批量修正 |
