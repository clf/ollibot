structure WebServer = Web(OllibotWebHandler)

structure WebLoop = struct

  val () = WebServer.go(SOME 5315)

end
