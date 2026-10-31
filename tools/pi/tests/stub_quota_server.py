#!/usr/bin/env python3
"""额度耗尽（HTTP 429）stub server，用于 gf pi 的用量上限识别测试。

用法: python3 stub_quota_server.py <响应体> <请求日志文件>

启动后向 stdout 打印 "PORT:<端口>"，随后对每个收到的 POST 追加一行到日志文件。
"""

import sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

BODY = (
    sys.argv[1]
    if len(sys.argv) > 1
    else '{"error":{"code":"1302","message":"您已达到5小时使用上限，请在 5 小时后重试。"}}'
)
LOGFILE = sys.argv[2] if len(sys.argv) > 2 else ""


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def do_POST(self):  # noqa: N802 (BaseHTTPRequestHandler 约定)
        length = int(self.headers.get("content-length", 0) or 0)
        if length:
            self.rfile.read(length)
        if LOGFILE:
            with open(LOGFILE, "a", encoding="utf-8") as fp:
                fp.write(self.path + "\n")
        payload = BODY.encode("utf-8")
        self.send_response(429)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(payload)))
        self.end_headers()
        self.wfile.write(payload)

    def log_message(self, *args):
        pass


def main():
    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    print("PORT:%d" % server.server_address[1], flush=True)
    server.serve_forever()


if __name__ == "__main__":
    main()
