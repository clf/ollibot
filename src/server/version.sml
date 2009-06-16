structure Version =
struct

  val version = "Server 5, alpha-test"

  fun date () =
    Date.fmt "%a, %d %b %Y %H:%M:%S %Z" (Date.fromTimeLocal (Time.now ()))

end
