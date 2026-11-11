# (liii pi code all-in-one probe) 模块函数分类索引

`(liii pi code all-in-one probe)` 通过独立 `gf pi -p --no-session --no-tools --no-skills` 子进程向候选节点发送 `ping`。预检不恢复或写入主会话，不加载工具与技能；失败统一追加到配置目录下的按天日志 `logs/pi-YYYY-MM-DD.log`。

使用时导入：

```scheme
(import (liii pi code all-in-one probe))
```

## 查看函数文档

```bash
gf doc pi-probe-node
gf doc pi-probe-error-code
gf doc pi-probe-prompt
```

## 函数分类索引

- `pi-probe-node` - 后台脚本模式预检节点，支持隔离配置、超时与测试运行器注入
- `pi-probe-error-code` - 从预检错误文本提取 HTTP 状态码、timeout 或 error
- `pi-probe-prompt` - 微型预检提示词常量 `ping`
