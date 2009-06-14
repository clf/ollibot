## A tiny HTTP server

from BaseHTTPServer import HTTPServer
from BaseHTTPServer import BaseHTTPRequestHandler
import os.subprocess

class myHandler(BaseHTTPRequestHandler):

    def do_WELCOME(self):
        self.wfile.write("""<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html 
     PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
     "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">

<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Rob Simmons</title>
<link rel="stylesheet" type="text/css" href="rob.css" media="screen,projection" />
</head>

<body><p>Hello, world</p>""")
        self.wfile.write("<p>GET string: a" + self.path + "b</p>" )
        self.wfile.write("""</body></html>""")

    def do_CUSTOM(self,title,subtitles,content):
        f = open('server/ollibot.html', 'r')
        s = f.read()
        s = s.replace('<?pagetitle?>',title)
        s = s.replace('<?title?>','<h1>' + title + '</h1>')
        s = s.replace('<?subtitle?>',subtitles)
        s = s.replace('<?content?>',content)
        self.wfile.write(s)

    def do_GET(self):
        if("/" == self.path):
            self.printCustomHTTPResponse(200)
            self.do_CUSTOM("Main Page","","Hello, world!")
        elif("/ollibot.css" == self.path):
            self.printCustomTXTResponse(200)
            f = open('server/ollibot.css', 'r')
            self.wfile.write(f.read())
        else:
            s = subprocess.getoutput("echo \"" + self.path[1:] + "\" " +
                                     "| sml -m src/web.cm -Dcm.verbose=0")
            self.printCustomHTTPResponse(200)
            self.do_CUSTOM("Page!","",s)

    def printBrowserHeaders(self):
        keys = self.headers.dict.keys()                                
        self.wfile.write("\n<ul>")
        for key in keys:
            self.wfile.write("\n<li><b>" + key + "</b>: ")
            self.wfile.write(self.headers.dict[key] + "\n</li>\n")
        self.wfile.write("</ul>\n")

    def printCustomHTTPResponse(self, respcode):
        self.send_response(respcode)
        self.send_header("Content-Type", "text/html")                 
        self.send_header("Server", "myHandler")
        self.end_headers()

    def printCustomTXTResponse(self, respcode):
        self.send_response(respcode)
        self.send_header("Content-Type", "text/css")                 
        self.send_header("Server", "myHandler")
        self.end_headers()


    def log_request(self, code='-', size='-'):
        user_agent = self.headers.dict['user-agent']
        self.log_message('"%s" %s %s %s',
                         self.requestline, str(code), str(size), user_agent)

if __name__ == "__main__":
    server = HTTPServer(('',2122), myHandler)                       
    for lp in range(5000):
        server.handle_request()
