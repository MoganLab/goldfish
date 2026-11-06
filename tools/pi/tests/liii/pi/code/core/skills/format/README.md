# (liii pi code core skills format) 技能格式化与命令展开测试导览

`(liii pi code core skills format)` 负责技能的三种文本格式化：注入系统
提示词的 `<skills>` XML 结构（含实体转义与 disable-model-invocation
过滤）、`/skill:<name>` 命令展开为完整技能块、以及技能块的逆向解析。

## 模块组成

- `tools/pi/liii/pi/code/core/skills/format.scm` — 格式化与展开实现

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/core/skills/format/
```

## 测试文件索引

- `format-skills-for-prompt-test.scm` — `format-skills-for-prompt`：XML 结构、实体转义与私有技能过滤
- `expand-skill-command-test.scm` — `expand-skill-command`：命中展开、参数透传与未命中回退
- `parse-skill-block-test.scm` — `parse-skill-block`：技能块逆向切分（name/content/user-message）
