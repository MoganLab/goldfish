# (liii pi code cli auth) 测试导览

`(liii pi code cli auth)` 提供了 Pi Coding Agent auth 子命令的参数解析、生效 API 密钥获取与输出功能。

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/cli/auth/
```

## 测试文件索引

- `parse-pi-auth-args-test.scm` — `parse-pi-auth-args`：解析 auth 子命令参数（识别 print-api-key、--provider 参数取值与错误校验）
- `pi-auth-print-api-key-test.scm` — `pi-auth-print-api-key`：解析并输出指定 provider 的生效 API 密钥
- `run-pi-auth-test.scm` — `run-pi-auth`：auth 子命令入口调度与错误处理
