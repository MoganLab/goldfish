# (liii json schema budget) 模块函数分类索引

`(liii json schema budget)` 提供单次校验的资源预算：
限制求值深度、Schema 节点评估数与引用解析次数，
避免递归 Schema 无限展开。

预算在每次校验开始前复位，因此复用同一个校验器不会累积预算。

## 常见用法示例

```scheme
(import (liii json schema budget))

(define budget (make-resource-budget 10 100 1000))
(budget-max-depth budget)   ; => 10
(budget-depth budget)       ; => 0
(enter-budget-depth! budget)
(budget-depth budget)       ; => 1
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/json/schema/budget "函数名"
```

## 函数分类索引

### 一、创建与判断
- `make-resource-budget` - 创建资源预算
- `resource-budget?` - 判断是否为资源预算
- `resource-budget-from-options` - 按选项创建资源预算

### 二、复位与计数
- `reset-resource-budget!` - 复位计数器
- `enter-budget-depth!` - 进入一层求值深度
- `leave-budget-depth!` - 退出一层求值深度
- `count-budget-step!` - 累加一次节点评估
- `count-budget-reference!` - 累加一次引用解析

### 三、读取
- `budget-depth` - 读取当前求值深度
- `budget-steps` - 读取已评估节点数
- `budget-references` - 读取已解析引用数
- `budget-max-depth` - 读取最大深度
- `budget-max-steps` - 读取最大评估数
- `budget-max-references` - 读取最大引用数

### 四、选项校验
- `validate-budget-options!` - 校验预算选项

## 错误处理约定

- 超过深度、评估数或引用数上限时抛出 `json-schema-budget-exceeded`
- `validate-budget-options!` 遇到非法预算选项时抛出 `json-schema-options-error`
