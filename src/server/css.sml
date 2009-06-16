structure CSS =
struct

  val csshead = 
      "<STYLE TYPE=\"text/css\">\n" ^

      "P { font: 10pt Verdana,Arial,Helvetica }\n" ^ 

      "TH { font: bold 10pt Verdana,Arial,Helvetica ; text-align: center}\n" ^ 

      "TD { margin : 1px; padding: 3px ; font: 10pt Verdana,Arial,Helvetica }\n" ^ 
      "BODY { font: 10pt Verdana,Arial,Helvetica }\n" ^ 

      "H1 { font : bold 16pt Verdana,Arial,Helvetica }\n" ^ 

      "TR.row_even { background : #DDFFDD }\n" ^
      "TR.row_odd  { background : #FFDDFF }\n" ^

      "A:link { color: #4444DD } \n" ^
      "A:visited { color: #9999FF } \n" ^
      "A:active { color: #DDDD44 } \n" ^

      "A.rundemo { font-size : 14px ; font-weight : bold }\n" ^

      "PRE { font : Fixedsys, Lucida Console, Courier, Monospace, Fixed }\n" ^

      "</STYLE>\n"


end