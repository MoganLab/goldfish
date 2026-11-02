# (liii goldcheck type-hint owner) 模块函数分类索引

A7 校验：hint 库中签名声明的归属唯一性，包括再导出过程的归属正确性、库内重复声明检查以及跨库重复声明检查。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、签名归属与重复校验
- `check-a7` - 校验单库内签名的归属正确性（A7a）与库内重复声明（A7b）
- `check-type-hint-duplicates` - 校验跨 hint 库之间的签名重复声明（A7c）
