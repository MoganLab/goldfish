# (liii goldcheck type-hint type) 模块函数分类索引

表达式分析类型推导：由 hint 签名与局部绑定推导表达式的类型，无法确定时返回 unknown（#f）。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、表达式类型推导
- `infer-type` - 推导表达式的分析类型（字面量、局部绑定、调用返回值）
- `call-return-type` - 计算候选签名中与实参兼容签名的公共返回类型
- `call-args-ok?` - 判断签名是否与调用实参列表兼容（数量与类型）
- `inline-assq-access?` - 判断表达式是否为 (cdr (assq ...)) 内联取值惯用法
