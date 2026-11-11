# (liii pi ai dispatch) 模块函数索引

`(liii pi ai dispatch)` 提供流式请求的协议路由派发层，根据服务商注册表或配置中的协议声明将请求派发至具体的 provider 流式实现。

使用时导入：

```scheme
(import (liii pi ai dispatch))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-request-provider-stream
```

## 函数分类索引

### 协议派发
- `pi-make-request-config` - 构造标准请求配置关联表，自动推导端点与协议
- `pi-request-provider-stream` - 根据协议类型路由并执行流式请求
