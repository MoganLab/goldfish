import http.server
import socketserver
import sys
import json
import time

class StubHandler(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        len_h = int(self.headers.get("content-length", 0))
        body = self.rfile.read(len_h) if len_h > 0 else b""
        req_json = {}
        if body:
            try:
                req_json = json.loads(body.decode("utf-8"))
            except:
                pass

        path = self.path

        # DeepSeek Search API: POST /v1/tools/search
        if "/v1/tools/search" in path:
            auth = self.headers.get("Authorization", "")
            if auth != "Bearer ds-test-key":
                self.send_response(401)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(json.dumps({"error": {"message": "Invalid API key", "type": "authentication_error"}}).encode("utf-8"))
                return
            q = req_json.get("query", "")
            count = req_json.get("count", 10)
            if "error-500" in q:
                self.send_response(500)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(json.dumps({"error": {"message": "Internal Server Error", "type": "server_error"}}).encode("utf-8"))
                return
            if "slow search" in q:
                time.sleep(3)
            if "empty results" in q:
                results = []
            elif "goldfish" in q:
                results = [
                    {
                        "title": "Goldfish Official",
                        "url": "https://goldfish.liii.pro",
                        "site_name": "liii.pro",
                        "snippet": "Goldfish Scheme 官网",
                        "summary": "Goldfish Scheme is a minimalist Scheme implementation with rich libraries.",
                        "content": "",
                        "date": "2026-01-01T00:00:00Z",
                        "rerank_score": 2.5,
                        "authority": 3
                    },
                    {
                        "title": "GitHub Goldfish",
                        "url": "https://github.com/MoganLab/goldfish",
                        "site_name": "github.com",
                        "snippet": "Goldfish Scheme source repository",
                        "summary": "Goldfish Scheme 的源代码仓库，托管在 GitHub。",
                        "content": "",
                        "date": None,
                        "rerank_score": 1.8,
                        "authority": 2
                    }
                ]
            else:
                results = [
                    {
                        "title": "Generic Result",
                        "url": "https://example.com/generic",
                        "site_name": "example.com",
                        "snippet": "A generic search result",
                        "summary": "A generic search result summary for the query: " + q,
                        "content": "",
                        "date": None,
                        "rerank_score": 1.0,
                        "authority": 1
                    }
                ]
            resp = {"query": q, "count": count, "results": results}
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        # DeepSeek Fetch API: POST /v1/tools/fetch
        if "/v1/tools/fetch" in path:
            auth = self.headers.get("Authorization", "")
            if auth != "Bearer ds-test-key":
                self.send_response(401)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(json.dumps({"error": {"message": "Invalid API key", "type": "authentication_error"}}).encode("utf-8"))
                return
            url = req_json.get("url", "")
            if "404" in url:
                self.send_response(404)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(json.dumps({"error": {"message": "Page not found", "type": "not_found"}}).encode("utf-8"))
                return
            resp = {"title": "Example Page", "content": "# Example\nFetched page content for " + url + " with some markdown text."}
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        self.send_response(404)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(b'{"error": {"message": "not found"}}')

    def log_message(self, format, *args):
        pass

class ThreadedTCPServer(socketserver.ThreadingMixIn, socketserver.TCPServer):
    daemon_threads = True
    allow_reuse_address = True

def main():
    httpd = ThreadedTCPServer(("127.0.0.1", 0), StubHandler)
    port = httpd.server_address[1]
    # Print port to stdout and flush so child process waiter can read it
    sys.stdout.write(f"PORT:{port}\n")
    sys.stdout.flush()
    httpd.serve_forever()

if __name__ == "__main__":
    main()
