# (liii pi web-search execute) 模块函数分类索引

`(liii pi web-search execute)` 提供 `web_search` 工具的执行入口，负责参数校验、鉴权、后端选择与结果组装。

使用时导入：

```scheme
(import (liii pi web-search execute))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc pi-execute-web-search
```

## 函数分类索引

### 工具执行
- `pi-execute-web-search` - 执行联网搜索工具调用，校验参数、鉴权并返回搜索结果
