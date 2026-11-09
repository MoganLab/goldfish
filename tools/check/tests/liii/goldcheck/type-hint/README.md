# (liii goldcheck type-hint) 模块函数分类索引

type-hint 规则顶层接口，校验 goldfish/hint/ 下 hint 库的结构与签名合法性，支持全量扫描、跨库重复签名检查及覆盖率状态统计展示。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、Hint 库扫描与校验
- `scan-hint-libs` - 扫描仓库中全部 hint 库文件路径
- `check-type-hint` - 对单个 hint 库文件执行完整的规则校验
- `check-type-hint-all` - 对全量 hint 库执行校验并聚合违规
- `check-type-hint-duplicates` - 检查跨 hint 库之间是否存在同名签名的重复声明（由 owner 子模块实现）
- `check-type-hint-file` - 对任意源码文件做 type-hint 检查的顶层入口

### 二、覆盖状态展示
- `type-hint-status-lines` - 生成 hint 库覆盖状态的摘要文本行列表
- `display-type-hint-statuses` - 向终端打印 hint 库覆盖状态摘要
