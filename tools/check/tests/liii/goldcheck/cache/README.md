# (liii goldcheck cache) 模块函数分类索引

gf check 规则级缓存，用于跳过未修改且上一次检查已通过的规则校验。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、规则缓存管理
- `rule-cache-path` - 计算规则缓存文件的绝对路径
- `rule-cache-hit?` - 检测规则缓存是否命中（文件存在且修改时间晚于源文件）
- `rule-cache-touch` - 创建或更新规则缓存文件
