# (liii goldcheck only) 模块函数分类索引

goldcheck 的 `--only` 规则过滤配置生成器，用于根据用户指定的单一规则限制检查配置。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、规则配置限制
- `restrict-check-config` - 改写配置使仅指定的规则处于 enabled 状态，保持原有 excludes 配置
- `single-rule-config` - 构造仅启用指定规则的新配置，保留该规则自身的字段
