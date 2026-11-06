# (liii subprocess) 模块函数分类索引

`(liii subprocess)` 是子进程功能的统一入口，重导出命令执行、组合、注册管理、进程控制、流式输出和错误记录等接口。各函数的文档与单元测试按实际实现所在的子模块组织。

## 查看函数文档

```bash
gf doc liii/subprocess
```

或者查看具体子模块及函数的文档和用例：

```bash
gf doc liii/subprocess/run "run-either"
```

## 功能分类索引

### 一、命令执行与组合
- `run`、`run-either`：执行命令并返回退出码或 Either 结果，文档见 `run/`
- `run-and`、`run-or`、`run-sequence`、`run-pipe`：串联多个命令，文档见 `combinator/`
- `run-if`、`run-when`：按条件选择或跳过命令，文档见 `combinator/`

### 二、命令注册与权限
- `run-set!`、`run-get`：注册和查询符号命令，文档见 `registry/`
- `run-allow!`、`run-ban!`、`run-unban!`：管理命令白名单与禁止规则，文档见 `registry/`

### 三、进程生命周期与 I/O
- `process-spawn`、`process-wait`、`process-poll`：创建进程并等待或查询退出状态
- `process-terminate`、`process-kill`、`process-kill-tree`：终止进程
- `process-read`、`process-write`、`process-close`：进行进程通信并释放资源
- 其他进程接口与选项辅助函数的文档见 `process/`

### 四、流式输出与错误处理
- `run-streaming`、`run-streaming-chunks`：流式处理子进程输出，文档见 `streaming/`
- `make-process-error`、`process-error?`、`process-error-message`：构造、识别和读取错误记录，文档见 `process-error/`
