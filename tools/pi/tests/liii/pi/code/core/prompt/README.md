# (liii pi code core prompt) 系统提示词组装测试导览

`(liii pi code core prompt)` 组装 `gf pi` 的系统提示词：preamble、
内置工具清单与指引、规则区段、Agent Skills 的 `<skills>` 轻量披露、
追加区段与 override 静默，以及由 stdin/文件参数构建初始提示消息。
项目上下文相关的 `pi-load-project-context`/`pi-resolve-prompt-input`
由子库 `(liii pi code core prompt context)` 提供，测试见 `context/`。

## 模块组成

- `tools/pi/liii/pi/code/core/prompt.scm` — 系统提示词组装与初始消息构建
- `tools/pi/liii/pi/code/core/prompt/context.scm` — 项目上下文加载（测试见 `context/`）

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/core/prompt/
```

## 测试文件索引

- `pi-build-system-prompt-test.scm` — `pi-build-system-prompt`：区段组装与 override 静默
- `pi-build-system-prompt-skills-test.scm` — `pi-build-system-prompt`：技能列表注入 `<skills>` 区块
- `build-initial-prompt-test.scm` — `build-initial-prompt`：stdin 与文件参数合成初始消息
- `PI_BUILTIN_TOOL_SNIPPETS-test.scm` — `PI_BUILTIN_TOOL_SNIPPETS`：内置工具片段常量
- `PI_BUILTIN_TOOL_GUIDELINES-test.scm` — `PI_BUILTIN_TOOL_GUIDELINES`：内置工具指引常量
