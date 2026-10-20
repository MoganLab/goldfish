# 064 差分渲染闪烁：复现证据

## 结论

闪烁已在真实 Windows Terminal 中复现，不是单张截图，而是连续帧里的
**整行瞬时空白**：

- 运行 `demo/tui/snake.scm`，最大化 Windows Terminal，连拍约 15～16 fps；
- 4 秒 61 帧中检测到 **10 次**整行空白事件（每次持续约一帧，65 ms 量级）；
- 空白行固定为 `y=271..308`（右侧 STATS 框的 `Heartbeat` 行）；
- 该行在前后两帧的墨迹量均为 125/126 像素，说明内容完整消失后又完整出现；
- 事件约每 7 帧出现一次（约 450 ms），与心跳行的刷新节奏吻合。

`flicker-triptych.png` 是同一位置 before / blank / after 三帧的拼图：
中间帧里 `Heartbeat` 整行（连同该行边框）消失，上下相邻行保持不变。

## 机制证据（ANSI 写入流）

运行 3 秒捕获原始 ANSI 流，得到 28 个渲染帧、53 次如下序列：

```text
CUR(row,1)  ESC[2K\r  CUR(row,1)  <新行文本>
```

即当前实现每次更新都是**先清整行、再移动光标、再写文本**。把流按每次
写入/清行重放到虚拟屏幕并逐操作快照，28 帧中在被更新行上出现 **40 次**
空白中间态。终端只要在 `ESC[2K` 与文本写入之间绘制一次，就会看到整行闪空。
每帧还会重复发送 hide-cursor（28 帧 28 次），这是次要嫌疑。

## 复现方法

```bash
# 真实终端连拍 + 自动检测空白中间帧
python3 tools/pi/devel/064-capture.py --seconds 4
```

脚本行为：

1. 用 `wt.exe -w -1 new-tab -d <repo> --title SnakeRepro ...` 直接启动 demo，
   **不经过 pynput 模拟键入**（中文输入法会把 `cd` 等按键合成候选词）；
2. 最大化新窗口，用 Pillow `ImageGrab` 连续抓取约 15～20 fps；
3. 对每帧按 y 统计亮像素：若某条 ≥10 px 高的文本带在相邻两帧都有内容、
   当前帧却完全空白，记为一次闪烁事件；
4. 保存 before / blank / after 与拼图，并输出事件清单。

复现脚本不依赖产品代码内部探针，修复前后可直接用同一命令对比。

## 修复后的验收

- `064-capture.py --seconds 4` 报告 **0 次 blank-band event**；
- `virtual-terminal` 的原始 ANSI 断言：
  - 更新行不再出现「clear-line 后紧跟同位置 update」；
  - 未变化行零写入；
  - 新行更短时只清除行尾；
  - 每帧的 DECSET 2026 同步边界成对出现；
- 逐写入重放不再产生被更新行的空白中间态。

## 文件

| 文件 | 说明 |
|---|---|
| `real-capture-summary.txt` | 61 帧、10 次事件的原始清单 |
| `real-mid.png` | 游戏运行中的整窗参考帧 |
| `real-event00-{before,blank,after}.png` | 第一次事件的相邻三帧 |
| `real-event01-{before,blank,after}.png` | 第二次事件的相邻三帧 |
| `flicker-triptych.png` | 上述三帧的拼图（最直观） |
| `../064-capture.py` | 可重复执行的连拍检测脚本 |
