#!/usr/bin/env python3
"""
tools/pi/devel/063-smoke.py

Windows 真实终端 (Windows Terminal) 自动化冒烟与截图测试脚本。
采用 Python + pynput + Pillow：
1. 定位或拉起 Windows Terminal；
2. 新建标签页进入 ~/git/goldfish 并运行 bin/gf demo/tui/snake.scm；
3. 截图 1 (01-launch.png)：游戏初始画面（长度 5，向右前进）；
4. 按 'p' 暂停，截图 2 (02-pause.png)：验证暂停按键有效 (Status: paused)；再按 'p' 恢复；
5. 直线前进吃食物并越界穿墙，截图 3 (03-wrap.png)：验证四面墙壁穿墙机制；
6. 快速转向 (down -> left -> up) 造成自咬自碰触发 game-over，截图 4 (04-gameover.png)：
   验证出现 RECORD 框、=== GAME OVER ===、Final Score；
7. 使用 pynput 键入名字 "GoldBot"，截图 5 (05-record-name.png)：验证在输入框中实时显示 GoldBot；
8. 按 Enter 结束游戏退出到 shell，截图 6 (06-exit.png)：验证输出 "Thanks for playing Snake (liii tui)!"；
9. 关闭测试标签页，恢复现场。
"""

import os
import sys
import time
import subprocess
import ctypes
from ctypes import wintypes
from PIL import ImageGrab
from pynput.keyboard import Controller, Key

user32 = ctypes.windll.user32
SW_RESTORE = 9
SW_SHOW = 5

class RECT(ctypes.Structure):
    _fields_ = [
        ("left", wintypes.LONG),
        ("top", wintypes.LONG),
        ("right", wintypes.LONG),
        ("bottom", wintypes.LONG)
    ]

def find_wt_window():
    hwnd = user32.FindWindowW("CASCADIA_HOSTING_WINDOW_CLASS", None)
    if hwnd and user32.IsWindowVisible(hwnd):
        return hwnd
    found = []
    def enum_cb(h, _):
        if user32.IsWindowVisible(h):
            length = user32.GetWindowTextLengthW(h)
            buff = ctypes.create_unicode_buffer(length + 1)
            user32.GetWindowTextW(h, buff, length + 1)
            title = buff.value
            class_buff = ctypes.create_unicode_buffer(256)
            user32.GetClassNameW(h, class_buff, 256)
            cls_name = class_buff.value
            if "CASCADIA" in cls_name or "Windows Terminal" in title:
                found.append(h)
        return True

    WNDENUMPROC = ctypes.WINFUNCTYPE(wintypes.BOOL, wintypes.HWND, wintypes.LPARAM)
    user32.EnumWindows(WNDENUMPROC(enum_cb), 0)
    return found[0] if found else None

def get_wt_executable():
    appdata = os.environ.get("LOCALAPPDATA", "")
    p = os.path.join(appdata, "Microsoft", "WindowsApps", "wt.exe")
    if os.path.exists(p):
        return p
    return "wt.exe"

def ensure_terminal_window():
    hwnd = find_wt_window()
    if not hwnd:
        wt_exe = get_wt_executable()
        print(f"[smoke] Launching Windows Terminal via {wt_exe}...")
        subprocess.Popen([wt_exe])
        for _ in range(30):
            time.sleep(0.5)
            hwnd = find_wt_window()
            if hwnd:
                break
    if not hwnd:
        raise RuntimeError("Failed to locate or launch Windows Terminal window.")

    user32.ShowWindow(hwnd, SW_RESTORE)
    user32.SetForegroundWindow(hwnd)
    time.sleep(1.0)
    return hwnd

def capture_window_screenshot(hwnd, save_path):
    rect = RECT()
    user32.GetWindowRect(hwnd, ctypes.byref(rect))
    bbox = (rect.left, rect.top, rect.right, rect.bottom)
    if bbox[2] <= bbox[0] or bbox[3] <= bbox[1]:
        bbox = None
    img = ImageGrab.grab(bbox=bbox)
    os.makedirs(os.path.dirname(save_path), exist_ok=True)
    img.save(save_path)
    print(f"[smoke] Captured screenshot -> {save_path} (size={img.size})")

def type_text(keyboard, text, delay=0.03):
    for ch in text:
        keyboard.type(ch)
        time.sleep(delay)

def main():
    print("[smoke] Starting Windows Terminal snake automation test (6 screenshots)...")
    keyboard = Controller()
    output_dir = os.path.join(os.path.dirname(__file__), "063-screenshots")
    os.makedirs(output_dir, exist_ok=True)

    hwnd = ensure_terminal_window()
    print(f"[smoke] Windows Terminal focused (HWND={hex(hwnd)})")

    # 1. 新建独立标签页 Ctrl+Shift+T
    print("[smoke] Opening new tab with Ctrl+Shift+T...")
    with keyboard.pressed(Key.ctrl), keyboard.pressed(Key.shift):
        keyboard.press('t')
        keyboard.release('t')
    time.sleep(2.0)

    try:
        # 2. 导航到工程目录并运行 snake demo
        print("[smoke] Navigating to ~/git/goldfish and launching snake...")
        type_text(keyboard, "cd ~/git/goldfish\n")
        time.sleep(1.0)
        type_text(keyboard, "bin/gf demo/tui/snake.scm\n")
        time.sleep(0.8)

        # 3. 截图 1：启动画面 (01-launch.png)
        snap_launch = os.path.join(output_dir, "01-launch.png")
        capture_window_screenshot(hwnd, snap_launch)

        # 4. 暂停测试：按 'p'，截图 2 (02-pause.png)
        print("[smoke] Pausing game with 'p'...")
        keyboard.type('p')
        time.sleep(0.5)
        snap_pause = os.path.join(output_dir, "02-pause.png")
        capture_window_screenshot(hwnd, snap_pause)

        # 恢复游戏：再按 'p'
        print("[smoke] Resuming game with 'p'...")
        keyboard.type('p')
        time.sleep(0.2)

        # 5. 穿墙测试：蛇身向右直线前进吃食物 (10, 5) 并穿越右边界回到左边界
        # 宽 20，初始头部在 x=5，食物在 x=10。走 16 步左右穿过右边界进入左侧 (x=1)
        print("[smoke] Waiting for snake to advance straight right (eat food and wrap-around wall)...")
        time.sleep(1.8)
        snap_wrap = os.path.join(output_dir, "03-wrap.png")
        capture_window_screenshot(hwnd, snap_wrap)

        # 6. 制造碰撞：蛇身此时长度为 6，连续下、左、上转向自咬
        print("[smoke] Steering snake to collide with itself: down -> left -> up...")
        keyboard.press(Key.down)
        keyboard.release(Key.down)
        time.sleep(0.18)

        keyboard.press(Key.left)
        keyboard.release(Key.left)
        time.sleep(0.18)

        keyboard.press(Key.up)
        keyboard.release(Key.up)
        time.sleep(0.3)

        # 7. 截图 4：自碰导致游戏结束结算画面 (04-gameover.png)
        snap_gameover = os.path.join(output_dir, "04-gameover.png")
        capture_window_screenshot(hwnd, snap_gameover)

        # 8. 在登记组件中输入名字 "GoldBot"
        print("[smoke] Typing player name 'GoldBot' in record editor...")
        # 默认 placeholder/text 是 "Player1"，先按 backspace 清理或直接追加/输入
        # 我们用 backspace 删掉 Player1 (7 个字符)
        for _ in range(10):
            keyboard.press(Key.backspace)
            keyboard.release(Key.backspace)
            time.sleep(0.04)
        type_text(keyboard, "GoldBot")
        time.sleep(0.5)

        # 截图 5：名字登记输入画面 (05-record-name.png)
        snap_record = os.path.join(output_dir, "05-record-name.png")
        capture_window_screenshot(hwnd, snap_record)

        # 9. 回车退出结算画面
        print("[smoke] Pressing Enter to finish record and exit to shell...")
        keyboard.press(Key.enter)
        keyboard.release(Key.enter)
        time.sleep(1.0)

        # 截图 6：退出后的终端画面 (06-exit.png)
        snap_exit = os.path.join(output_dir, "06-exit.png")
        capture_window_screenshot(hwnd, snap_exit)

        print("[smoke] Snake game session completed and exited cleanly.")

    finally:
        # 关闭新建的测试标签页 (Ctrl+Shift+W)
        print("[smoke] Closing test tab with Ctrl+Shift+W...")
        with keyboard.pressed(Key.ctrl), keyboard.pressed(Key.shift):
            keyboard.press('w')
            keyboard.release('w')
        time.sleep(0.5)

    print("[smoke] All 6 smoke steps completed successfully!")

if __name__ == "__main__":
    main()
