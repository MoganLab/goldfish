# (liii pi ai limits) 模块函数索引

`(liii pi ai limits)` 提供 AI 服务商用量上限（quota / usage limit）的统一识别与错误提示拼装，委托各 provider 注册的配额判定函数识别是否达到周期额度上限。

使用时导入：

```scheme
(import (liii pi ai limits))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-detect-usage-limit
```

## 函数分类索引

### 额度耗尽检测与提示
- `pi-detect-usage-limit` - 识别响应体是否达到服务商用量上限
- `pi-usage-limit-error?` - 判断响应是否属于额度耗尽错误（真值形式）
- `pi-usage-limit-error-message` - 生成面向用户的额度耗尽提示文本
- `pi-usage-limit-kind` - 获取额度耗尽判定结果的窗口类型
