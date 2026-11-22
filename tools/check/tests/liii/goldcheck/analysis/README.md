# (liii goldcheck analysis) 模块函数分类索引

只读库分析入口，聚合库解析、导入规范化、再导出归属和过程 arity 查询。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

- `resolve-library` - 解析库名并读取对外导出名
- `normalize-import-mapping` - 规范化导入修饰形式
- `reexport-owner` - 查询再导出符号的唯一所有者
- `procedure-arity` - 查询过程的参数 arity
