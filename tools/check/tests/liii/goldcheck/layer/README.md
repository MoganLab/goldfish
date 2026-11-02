# (liii goldcheck layer) 模块函数分类索引

架构分层规则检查核心实现，包含库名解析、import 依赖提取与分层依赖合规检查。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、分层规则检查
- `check-layer-order` - 检查源码文件集合是否违反分层依赖规则
- `file-imports` - 从源文件读取所有 import 的库名
- `path->lib-name` - 将源码文件相对路径推导为库名
