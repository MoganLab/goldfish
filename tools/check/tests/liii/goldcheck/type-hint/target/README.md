# (liii goldcheck type-hint target) 模块函数分类索引

目标文件与 hint 库集合的关联分析：检查目标库 import 闭包中各依赖库 hint 的结构（第二层检查），以及推导检查目标时实际加载类型环境的 hint 集合（状态行按需显示）。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、依赖 hint 结构检查
- `check-dep-hints` - 检查目标库 import 闭包中各依赖库 hint 的结构违规，违规归属被查文件

### 二、可达 hint 集合
- `reachable-hint-files` - 返回检查目标文件时实际加载类型环境的 hint 库文件列表（去重）
