# (liii goldcheck report) 模块函数分类索引

gf check 结果输出与格式化，用于终端格式化展示单条规则校验结果、违规列表及退出状态码。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、规则结果与格式化
- `format-rule-line` - 格式化单条规则检查结果的行字符串（OK/FAILED、名称、耗时）
- `display-rule-result` - 输出单条规则检查结果到终端

### 二、违规报告与退出
- `violation-report-lines` - 生成格式化的违规详情行列表
- `violation-display-hint` - 生成超出上限截断时的提示说明行
- `report-and-exit` - 统计所有规则执行结果，输出报告并以对应状态码退出
