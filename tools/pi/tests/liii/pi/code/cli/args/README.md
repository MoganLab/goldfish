# (liii pi code cli args) 命令行参数解析测试导览

`(liii pi code cli args)` 解析 `gf pi` 的命令行参数：布尔开关、带值参数、
可重复参数（`--skill`/`--append-system-prompt`）、位置消息与未知标志
收集，以及会话名规范化与 review 类标志组合校验。

## 模块组成

- `tools/pi/liii/pi/code/cli/args.scm` — 参数解析实现

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/cli/args/
```

## 测试文件索引

- `parse-pi-args-test.scm` — `parse-pi-args`：默认值、带值/可重复参数、位置消息与未知标志
- `pi-normalize-session-name-test.scm` — `pi-normalize-session-name`：换行折叠与无效输入
- `pi-validate-review-flags-test.scm` — `pi-validate-review-flags`：合法通过与非法组合抛错
- `skills-args-test.scm` — 技能参数 `--skill`/`--skills`/`--no-skills` 解析
