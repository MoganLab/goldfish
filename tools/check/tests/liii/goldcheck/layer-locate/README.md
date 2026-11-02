# (liii goldcheck layer-locate) 模块函数分类索引

架构分层规则中的库与域定位逻辑，支持按特异性在域表中检索库名归属的域与分层。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、域与层定位
- `domain-name-of-lib` - 查询库名在域表中命中的域名
- `layer-name-of-lib` - 查询库名在域表中命中的层名
- `%locate` - 在域表中按特异性定位库名的域、层与层索引
- `%locate-layer-in` - 在指定域内定位库名所在层与索引
- `%domain-for-path` - 根据文件路径前缀推导所属域
