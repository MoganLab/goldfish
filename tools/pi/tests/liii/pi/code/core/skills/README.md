# (liii pi code core skills) Agent Skills 门面测试导览

`(liii pi code core skills)` 是技能体系的统一导出门面，聚合子模块
`types`（记录与规范校验）、`frontmatter`（YAML 解析）、`discovery`
（目录发现）与 `format`（格式化与命令展开），各子模块的单元测试位于
对应子目录。

## 模块组成

- `tools/pi/liii/pi/code/core/skills.scm` — 门面 re-export
- `tools/pi/liii/pi/code/core/skills/types/` — 记录与校验（测试见 `types/`）
- `tools/pi/liii/pi/code/core/skills/frontmatter.scm` — frontmatter 解析（测试见 `frontmatter/`）
- `tools/pi/liii/pi/code/core/skills/discovery.scm` — 目录发现（测试见 `discovery/`）
- `tools/pi/liii/pi/code/core/skills/format.scm` — 格式化与展开（测试见 `format/`）

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/core/skills/
```
