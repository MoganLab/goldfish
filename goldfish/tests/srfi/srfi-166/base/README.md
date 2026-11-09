# (srfi srfi-166 base) 模块函数分类索引

`(srfi srfi-166 base)` 是 SRFI-166 Monadic Formatting 的核心运行时：
格式化对象是「等被调用时才向环境索取端口、行列、宽度」的组合子，
必须经 `show` 求值才产生输出。`(liii format base)` 以 Goldfish
命名重导出本库全部函数，日常使用推荐导入该库。

## 常见用法示例

```scheme
(import (srfi srfi-166 base))

(show #f "hello " "world")            ; => "hello world"
(show #f (padded 6 "abc"))            ; => "   abc"
(show #f (joined each '(1 2 3) " "))  ; => "1 2 3"
(show #f (with ((precision 2)) 3.14159)) ; => "3.14"
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc srfi/srfi-166/base "函数名"
```

## 函数分类索引

### 一、运行入口
- `show` - 把格式化对象渲染到端口或字符串
- `call-with-output` - 捕获子组合子的输出再交给消费者
- `output-default` - 默认输出过程，负责推进行列状态

### 二、对象输出
- `displayed` - 按 display 语义输出
- `written` - 按 write 语义输出并标注共享/循环结构
- `written-shared` - 只标注共享结构
- `written-simply` - 不标注共享结构
- `escaped` - 按引号与转义规则输出字符串
- `maybe-escaped` - 满足谓词时才转义
- `numeric` - 数值格式化（基数、精度、千分位）
- `numeric/comma` - 只加千分位分隔符
- `numeric/si` - 用 SI 词头压缩量级
- `numeric/fitted` - 数值定宽输出

### 三、空白与定位
- `nl` - 输出换行
- `fl` - 需要时输出换行
- `nothing` - 不输出任何内容
- `space-to` - 推进到指定列
- `tab-to` - 推进到下一个制表位

### 四、组合与连接
- `each` - 顺序输出若干对象
- `each-in-list` - 顺序输出列表元素
- `joined` - 用分隔符连接列表元素
- `joined/prefix` - 首元素前也输出分隔符
- `joined/suffix` - 末元素后也输出分隔符
- `joined/last` - 用独立格式输出末元素
- `joined/dot` - 处理 improper list 的尾部
- `joined/range` - 按整数区间生成元素

### 五、宽度控制
- `padded` - 左侧填充到目标宽度
- `padded/right` - 右侧填充到目标宽度
- `padded/both` - 两侧填充到目标宽度
- `trimmed` - 保留尾部裁剪到目标宽度
- `trimmed/right` - 保留头部裁剪到目标宽度
- `trimmed/both` - 保留中间裁剪到目标宽度
- `trimmed/lazy` - 直接丢弃超出部分
- `fitted` - 定宽输出（先填充后裁剪）
- `fitted/right` - 右侧对齐的定宽输出
- `fitted/both` - 两侧居中的定宽输出

### 六、组合子构造与环境
- `fn` - 从普通过程构造组合子并注入格式化环境
- `with` - 局部绑定状态变量
- `with!` - 就地修改当前环境的状态变量
- `forked` - 先求值副作用分支再继续主流程
- `make-state-variable` - 自定义状态变量
- `port` `row` `col` `width` `output` `writer` `pad-char`
  `ellipsis` `radix` `precision` `comma-rule` - 内置状态变量

## 错误处理约定

- `show` 的输出目标不是端口也不是 `#t` / `#f` 时抛出 `error`
- 宽度类组合子要求非负精确整数宽度，负宽度会抛出 `error`
