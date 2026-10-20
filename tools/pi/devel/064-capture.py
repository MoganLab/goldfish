#!/usr/bin/env python3
"""
tools/pi/devel/064-capture.py

非截图式的闪烁复现器：在真实 Windows Terminal 中运行贪吃蛇 demo，
以约 15-20 fps 连续抓取窗口，自动检测“整行瞬时空白”的中间帧。

为什么需要它：闪烁只持续几十毫秒，单张静态截图几乎必然错过。
本脚本把连续帧变成可统计、可留档的证据：
  - 相邻帧中同一文本行都非空，而当前帧整行空白 => 一次闪烁事件；
  - 保存 before / blank / after 三帧，并由调用方在修复后重跑对比。

注意：不要用 pynput 往终端里模拟键入命令，中文输入法会把 `cd` 等
按键组合成候选词。这里用 `wt.exe new-tab -d <repo>` 直接启动命令，
既不经过键盘，也不受输入法影响。

用法：
  python3 tools/pi/devel/064-capture.py --seconds 4
  python3 tools/pi/devel/064-capture.py --seconds 4 --out tools/pi/devel/064-repro

修复验收：同一命令在修复后应报告 0 次 blank-band event。
"""

import argparse
import ctypes
import os
import subprocess
import sys
import time
from ctypes import wintypes
from pathlib import Path

import numpy as np
from PIL import Image, ImageDraw, ImageGrab

user32 = ctypes.windll.user32
WM_CLOSE = 0x0010
SW_MAXIMIZE = 3
TITLE = "SnakeRepro"


class RECT(ctypes.Structure):
    _fields_ = [
        ("left", wintypes.LONG),
        ("top", wintypes.LONG),
        ("right", wintypes.LONG),
        ("bottom", wintypes.LONG),
    ]


def find_window(title=TITLE):
    found = []

    def callback(hwnd, _):
        if not user32.IsWindowVisible(hwnd):
            return True
        buf = ctypes.create_unicode_buffer(256)
        user32.GetWindowTextW(hwnd, buf, 256)
        if title in buf.value:
            found.append(hwnd)
        return True

    proc = ctypes.WINFUNCTYPE(wintypes.BOOL, wintypes.HWND, wintypes.LPARAM)
    user32.EnumWindows(proc(callback), 0)
    return found[0] if found else None


def launch_window(repo_root):
    wt = os.path.join(
        os.environ.get("LOCALAPPDATA", ""),
        "Microsoft",
        "WindowsApps",
        "wt.exe",
    )
    bash = r"C:\Program Files\Git\usr\bin\bash.exe"
    if not os.path.exists(bash):
        bash = "bash"
    subprocess.Popen(
        [
            wt,
            "-w",
            "-1",
            "new-tab",
            "-d",
            str(repo_root),
            "--title",
            TITLE,
            bash,
            "-lc",
            "bin/gf demo/tui/snake.scm",
        ]
    )
    for _ in range(40):
        time.sleep(0.25)
        hwnd = find_window()
        if hwnd:
            return hwnd
    raise RuntimeError("Windows Terminal window did not appear")


def capture(hwnd, seconds):
    user32.ShowWindow(hwnd, SW_MAXIMIZE)
    time.sleep(0.6)
    rect = RECT()
    user32.GetWindowRect(hwnd, ctypes.byref(rect))
    bbox = (rect.left, rect.top, rect.right, rect.bottom)
    frames, stamps = [], []
    start = time.perf_counter()
    while time.perf_counter() - start < seconds:
        frames.append(np.array(ImageGrab.grab(bbox=bbox).convert("L")))
        stamps.append(time.perf_counter() - start)
    fps = len(frames) / (stamps[-1] - stamps[0])
    return bbox, frames, fps


def detect_blank_bands(frames):
    """Return (frame_index, y0, y1, prev_ink, next_ink) for transient blank lines."""
    # Skip the title/tab strip above the terminal content.
    content = [f[80:, :] for f in frames]
    ink = np.array([(a > 100).sum(axis=1) for a in content])
    events = []
    for t in range(1, len(frames) - 1):
        prev, cur, nxt = ink[t - 1], ink[t], ink[t + 1]
        blank = cur < 3
        y, height = 0, cur.shape[0]
        while y < height:
            if not blank[y]:
                y += 1
                continue
            y0 = y
            while y < height and blank[y]:
                y += 1
            y1 = y
            if y1 - y0 >= 10:
                before = int(prev[y0:y1].max())
                after = int(nxt[y0:y1].max())
                if before > 20 and after > 20:
                    events.append((t, y0 + 80, y1 + 80, before, after))
    return events


def save_evidence(out_dir, frames, events):
    out_dir.mkdir(parents=True, exist_ok=True)
    Image.fromarray(frames[len(frames) // 2]).save(out_dir / "real-mid.png")
    for idx, (t, y0, y1, _, _) in enumerate(events[:2]):
        top = max(0, y0 - 140)
        bottom = min(frames[t].shape[0], y1 + 60)
        for label, frame_idx in (
            ("before", t - 1),
            ("blank", t),
            ("after", t + 1),
        ):
            crop = frames[frame_idx][top:bottom, :]
            Image.fromarray(crop).save(out_dir / f"real-event{idx:02d}-{label}.png")

    if events:
        top = max(0, events[0][1] - 140)
        bottom = min(frames[0].shape[0], events[0][2] + 60)
        crops = [
            Image.fromarray(frames[i][top:bottom, :]).convert("RGB")
            for i in (events[0][0] - 1, events[0][0], events[0][0] + 1)
        ]
        labels = ["before", "blank (flicker)", "after"]
        width = max(c.width for c in crops)
        height = sum(c.height + 28 for c in crops)
        sheet = Image.new("RGB", (width, height), (0, 0, 0))
        draw = ImageDraw.Draw(sheet)
        y = 0
        for label, crop in zip(labels, crops):
            draw.text((8, y + 6), label, fill=(255, 200, 80))
            y += 28
            sheet.paste(crop, (0, y))
            y += crop.height
        sheet.save(out_dir / "flicker-triptych.png")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--seconds", type=float, default=4.0)
    parser.add_argument("--out", default=str(Path(__file__).parent / "064-repro"))
    parser.add_argument("--keep-open", action="store_true")
    args = parser.parse_args()

    repo_root = Path(__file__).resolve().parents[3]
    hwnd = launch_window(repo_root)
    print(f"window hwnd={hex(hwnd)}")
    try:
        time.sleep(1.2)
        bbox, frames, fps = capture(hwnd, args.seconds)
        events = detect_blank_bands(frames)
        print(f"bbox={bbox} frames={len(frames)} fps={fps:.2f} events={len(events)}")
        for event in events:
            print(
                "  frame %d y=%d..%d prev_ink=%d next_ink=%d" % event
            )
        out_dir = Path(args.out)
        save_evidence(out_dir, frames, events)
        with open(out_dir / "real-capture-summary.txt", "w", encoding="utf-8") as f:
            f.write(f"frames={len(frames)} fps={fps:.2f} events={len(events)}\n")
            for event in events:
                f.write("frame %d y=%d..%d prev_ink=%d next_ink=%d\n" % event)
        print(f"evidence -> {out_dir}")
    finally:
        if not args.keep_open:
            user32.PostMessageW(hwnd, WM_CLOSE, 0, 0)
    return 0


if __name__ == "__main__":
    sys.exit(main())
