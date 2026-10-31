import http.server
import socketserver
import sys
import json
import time

class StubHandler(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        len_h = int(self.headers.get("content-length", 0))
        body = self.rfile.read(len_h) if len_h > 0 else b""

        if "/slow-stream" in self.path:
            # 发 1 个 chunk 后静默挂起：模拟 LLM 首 token 前的长思考期，
            # 用于验证 progress callback 能在无数据到达时探测中止信号
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.send_header("Cache-Control", "no-cache")
            self.end_headers()
            self.wfile.write(b"data: hello\n\n")
            self.wfile.flush()
            time.sleep(30)
            return

        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(json.dumps({"ok": True}).encode("utf-8"))

    def log_message(self, format, *args):
        pass

class ThreadedTCPServer(socketserver.ThreadingMixIn, socketserver.TCPServer):
    daemon_threads = True

def main():
    httpd = ThreadedTCPServer(("127.0.0.1", 0), StubHandler)
    port = httpd.server_address[1]
    sys.stdout.write(f"PORT:{port}\n")
    sys.stdout.flush()
    httpd.serve_forever()

if __name__ == "__main__":
    main()
