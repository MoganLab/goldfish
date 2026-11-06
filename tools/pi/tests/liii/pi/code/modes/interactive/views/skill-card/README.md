# (liii pi code modes interactive views skill-card) 技能折叠卡片测试导览

`(liii pi code modes interactive views skill-card)` 实现对标上游
`SkillInvocationMessageComponent` 的技能折叠卡片：折叠时单行紧凑条目
`[skill] name (Ctrl+O 展开)`，展开后显示标题与正文 Markdown，支持鼠标
点击切换；渲染走共享缓存（与 tool-card/thinking 卡片同款机制）。

## 模块组成

- `tools/pi/liii/pi/code/modes/interactive/views/skill-card.scm` — 卡片组件实现

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/modes/interactive/views/skill-card/
```

## 测试文件索引

- `make-skill-card-test.scm` — `make-skill-card`：构造与折叠/展开渲染
- `skill-card-p-test.scm` — `skill-card?`：类型谓词
- `skill-card-component-p-test.scm` — `skill-card-component?`：组件谓词
- `skill-card-expanded-p-test.scm` — `skill-card-expanded?`：展开状态读取
- `skill-card-set-expanded-bang-test.scm` — `skill-card-set-expanded!`：展开状态切换
