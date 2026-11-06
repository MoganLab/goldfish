# (liii packrat) 模块函数分类索引

`(liii packrat)` 提供 Packrat 解析算法：解析规则以宏定义，非终结符结果按位置记忆化，避免重复解析。

## 查看函数文档

```bash
gf doc liii/packrat
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/packrat "packrat-parser"
```

## 函数分类索引

### 一、解析结果
- `parse-result?` - 判断对象是否为解析结果记录
- `parse-result-successful?` - 读取解析结果是否成功
- `parse-result-semantic-value` - 读取解析结果的语义值
- `parse-result-next` - 读取解析结果的后续解析状态
- `parse-result-error` - 读取解析结果携带的错误
- `make-result` - 构造成功的解析结果记录
- `make-expected-result` - 构造「期望某记号」类型的失败结果
- `make-message-result` - 构造带错误消息的失败结果
- `merge-result-errors` - 把额外错误合并进解析结果
- `parse-error->parse-result` - 把解析错误转换为失败的解析结果

### 二、解析结果流
- `parse-results?` - 判断对象是否为解析结果流记录
- `parse-results-position` - 读取解析结果流的位置
- `parse-results-next` - 读取解析结果流的后续状态
- `parse-results-token-kind` - 读取当前记号的类型
- `parse-results-token-value` - 读取当前记号的值
- `parse-results-base` - 读取当前基础记号
- `base-generator->results` - 把取词生成器转换为解析结果流
- `prepend-base` - 构造带基础记号的解析结果流
- `prepend-semantic-value` - 构造带记忆化语义值的解析结果流
- `results->result` - 按非终结符名查询或登记记忆化结果

### 三、解析错误
- `parse-error?` - 判断对象是否为解析错误记录
- `parse-error-position` - 读取解析错误的位置
- `parse-error-expected` - 读取解析错误中期望的记号集合
- `parse-error-messages` - 读取解析错误中的消息列表
- `make-error-expected` - 构造「期望某记号」类型的解析错误
- `make-error-message` - 构造带错误消息的解析错误
- `parse-error-empty?` - 判断解析错误是否为空
- `merge-parse-errors` - 合并两个解析错误

### 四、解析位置
- `make-parse-position` - 构造解析位置记录
- `parse-position?` - 判断对象是否为解析位置记录
- `parse-position-file` - 读取解析位置的文件名
- `parse-position-line` - 读取解析位置的行号
- `parse-position-column` - 读取解析位置的列号
- `top-parse-position` - 构造文件起始位置
- `update-parse-position` - 按读入的字符推进解析位置
- `parse-position->string` - 把解析位置格式化为字符串
- `parse-position>?` - 比较两个解析位置的先后

### 五、组合子
- `packrat-check-base` - 检查当前基础记号类型的组合子
- `packrat-check` - 对子解析结果做后置处理的组合子
- `packrat-or` - 择一解析的组合子
- `packrat-unless` - 排除某个分支的组合子

### 六、宏
- `packrat-parser` - 定义 packrat 解析规则

## 说明

- packrat 支持右递归，不支持左递归
- 规则中 `^` 绑定当前位置，`(! '记号)` 断言记号
- 完整算术表达式解析器的集成用例见 `packrat-parser-test.scm`
