# (liii http common) 模块函数索引

`(liii http common)` 存放 `(liii http)` 与 `(liii http async)` 共享的响应处理与参数校验辅助函数。

使用时导入：

```scheme
(import (liii http common))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc http-ok?
```

## 函数分类索引

### 响应处理
- `http-ok?` - 检查响应是否成功（2xx 状态码）

### 内部辅助函数（供 (liii http) 与 (liii http async) 复用）
- `http-require-string` - 校验必填字符串参数
- `http-require-procedure` - 校验必填过程参数
- `http-require-boolean` - 校验必填布尔参数
- `http-optional-string` - 校验可选字符串参数
- `http-optional-procedure` - 校验可选过程参数
- `http-optional-timeout` - 校验可选超时参数
- `http-scalar->string` - 标量值转字符串
- `http-normalize-string-alist` - 规范化字符串关联列表
- `http-normalize-files` - 规范化文件上传参数
- `http-normalize-post-form-data` - 规范化 POST 表单数据
