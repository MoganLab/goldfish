# (liii pi code core prompt context) 项目上下文加载测试导览

`(liii pi code core prompt context)` 负责系统提示词的项目上下文部分：
`AGENTS.md` 等项目文件的发现与加载，以及提示词输入（字面量/`@file`/
stdin）的解析。

## 模块组成

- `tools/pi/liii/pi/code/core/prompt/context.scm` — 项目上下文与提示词输入解析

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/core/prompt/context/
```

## 测试文件索引

- `pi-load-project-context-test.scm` — `pi-load-project-context`：项目上下文文件加载
- `pi-resolve-prompt-input-test.scm` — `pi-resolve-prompt-input`：提示词输入解析
