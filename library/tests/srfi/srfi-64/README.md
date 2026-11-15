# (srfi srfi-64) 模块函数分类索引

`(srfi srfi-64)` 提供了 Scheme 测试套件标准 API（SRFI-64），用于兼容使用 SRFI-64 的外部代码与测试套件。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc srfi/srfi-64 "函数名"
```

## 函数分类索引

### 一、测试组与执行控制
- `test-begin` - 开始一个测试组
- `test-end` - 结束当前测试组
- `test-group` - 定义并执行带独立作用域的测试组
- `test-group-with-cleanup` - 定义带清理动作的测试组
- `test-apply` - 在指定匹配器过滤下执行测试
- `test-with-runner` - 在指定运行器上下文中执行测试

### 二、断言宏
- `test-assert` - 断言表达式为真
- `test-eqv` - 使用 `eqv?` 断言等价
- `test-eq` - 使用 `eq?` 断言同一
- `test-equal` - 使用 `equal?` 断言结构相等
- `test-approximate` - 断言数值在误差范围内近似相等
- `test-error` - 断言表达式求值抛出异常
- `test-read-eval-string` - 读取字符串中的表达式并求值

### 三、匹配器与过滤
- `test-match-nth` - 匹配第 n 个及之后共 count 个测试
- `test-match-all` - 逻辑与组合匹配器
- `test-match-any` - 逻辑或组合匹配器
- `test-match-name` - 按测试名称精确匹配
- `test-skip` - 跳过匹配的测试
- `test-expect-fail` - 标记匹配的测试预期失败

### 四、测试运行器与结果属性
- `test-runner?` - 判断是否为 test-runner
- `test-runner-null` - 创建空测试运行器
- `test-runner-simple` - 创建标准简单测试运行器
- `test-runner-current` - 获取或设置当前测试运行器
- `test-runner-factory` - 获取或设置默认运行器工厂
- `test-runner-get` - 获取当前测试运行器
- `test-runner-create` - 通过工厂创建新测试运行器
- `test-runner-reset` - 重置运行器状态与计数
- `test-runner-group-path` - 获取当前测试组路径
- `test-runner-test-name` - 获取当前测试名称
- `test-result-ref` / `test-result-set!` / `test-result-clear` / `test-result-remove` - 结果属性表操作
- `test-result-kind` / `test-passed?` - 获取测试结果类型与是否通过
