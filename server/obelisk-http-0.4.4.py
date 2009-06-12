#!/usr/bin/python
#Obelisk http 0.4.4

#Filipe Caldas 27/19/2007

try:
    import socket,thread,os,sys,popen2
    from time import localtime
except:
    print "Some lib isn't installed."
try:
    import psyco
    psyco.full()
except:
    pass

#HERE IS THE CONFIG OF YOUR SERVER#

#CONFIG 
BINDER='127.0.0.1'                       #where the socket will be binded with bind((BINDER,PORT))
PORT=8080                                #listen port
HTML_DIR=os.curdir+'/html'               #dir where is the html stuff (no / in end)
CGI_DIR=os.curdir+'/cgi-bin/'            #dir where CGI-BIN is (/ in end!)
BAAR='/index.html'                       #file to send if GET /
LOGFILE='c:/obelisk-log.txt'             #Acess/Error Log file
BUFF=512                                 #Size to receive in HEAD requests
PHP='c:/php-5.2.4-Win32/php.exe'         #Where is the PHP executable, '' to disable php
PHPBINDER='./pybind.php'                 #this will be the PHP var interp who will pass from environment to $_GET[varname](don't change unless you know what you are making)
#/CONFIG


######################################################
#Global variables
######################################################
VERSION="0.4.4"
IDENT="Obelisk-HTTP"


######################################################
#Main
######################################################

def main():
    port=PORT
    server=socket.socket(socket.AF_INET,socket.SOCK_STREAM)
    print "Obelisk Web Server "+VERSION+"\n\n"
    try:
        server.bind((BINDER,port))
    except:
        print "Error trying to bind to "+BINDER+":"+ str(port)
        raw_input("[Exit]")
        sys.exit(0)
    server.listen(5)
    print "Listening in "+ BINDER +":"+str(port)+"\n"
    client,address=server.accept()
    LogClient("Connection from " + str(address[0]) + " : " + str(address[1]))
    thread.start_new_thread(connection,(client,))
    while 1:
        client,address=server.accept()
        LogClient("Connection from " + str(address[0]) + " : " + str(address[1]))
        thread.start_new_thread(connection,(client,))

######################################################

######################################################

def connection(client):
    try:
        msg=client.recv(4) #head the method(POST,GET,HEAD?)
    except:
        LogClient("Connection Reset by Peer")
    if msg[0:3]=="GET":
        REQUEST_GET(client,msg)     #ok(absolutly no bugs)
    elif msg[0:4]=="POST":
        REQUEST_POST(client,msg)    #ok(popen2 is making me cry)
    elif msg[0:4]=="HEAD":
        REQUEST_HEAD(client,msg)
    else:
        SENDFILE(client,HTML_DIR+"/malformed.html","r",1)
    return 0

#####################################################

#####################################################

def LogClient(Message):
    try:
    	fd=open(LOGFILE,'a')
    except:
        print "Error: Can't write to "+LOGFILE
        print "Change the LOGFILE variable to an existent file with permissions to write!"
        return 1
    tm=localtime()
    fd.write(str(tm[3]) + ":" + str(tm[4]) + "  " + str(tm[2]) + '/' + str(tm[1]) + '/' + str(tm[0]) + ":" + str(Message) + "\r\n")
    fd.close()
    return(0)

#####################################################

#####################################################

def SENDFILE(client,filename,readmethod,entire):
    """Send a file to a socket readmethod=r/rb entire=0/1 read entire file?"""
    if entire==1:
        try:
            fd=open(filename, readmethod)
        except:
            return 1
        client.send(fd.read())
    else:
        try:
            fd=open(filename, readmethod)
        except:
            return 1        
        EOF=0
        while EOF!=1:
            LN=fd.readline()
            if LN!='':
                client.send(LN)
            else:
                fd.close()
                EOF=1
    return 0

#####################################################

#####################################################

def SENDFILE2(client,filename,readmethod,entire,HEAD):
    """Send a file to a socket readmethod=r/rb entire=0/1 read entire file?"""
    """Request=Complete Server Request"""
    """This is a try to suport MS browsers"""
    if entire==1:
        try:
            fd=open(filename, readmethod)
        except:
            return 1
        bex=fd.read() #just read the file
        #this create a W3 header to file
        if bex[0:2]!="<!" and (filename[-3:]=='htm' or filename[-4:]=='html'):
            bex='<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">\n'+bex
        if HEAD!='':
            a=Decode_Header(HEAD,filename,len(bex))
            if a!=None:
                bex=a+bex
        client.send(bex)
    else:
        try:
            fd=open(filename, readmethod)
        except:
            return 1        
        EOF=0
        while EOF!=1:
            LN=fd.readline()
            if LN!='':
                client.send(LN)
            else:
                fd.close()
                EOF=1
    return 0

#####################################################

#####################################################

def Decode_Header(Header,Fname,LENBFF):
    """Receive a Get Request Header and a filename and try to interp"""
    """And return a response Header"""
    RETHEADER=''
    if Header=='':
        return ''
    else:
        dt=localtime()
        RETHEADER='HTTP/1.1 200 OK\nDate: ' + str(dt[2]) + ' ' +str(dt[1])+ ' ' + str(dt[0]) + ' ' +str(dt[3]) + ':' + str(dt[4]) + ':' + str(dt[5]) + "\n"
        RETHEADER=RETHEADER+'Server: ' + IDENT +'\n'
        RETHEADER=RETHEADER+'Content-Lenght: '+str(LENBFF)+"\n"
        if Header.find("Connection:")!=-1:
            RETHEADER=RETHEADER+'Connection: Keep-Alive\n'
            RETHEADER=RETHEADER+'Keep-Alive: timeout=5, max=100\n'
        return RETHEADER+"\n"
            
#####################################################

#####################################################


def Basic_Header(Header):
    """Receive a Get Request Header and a filename and try to interp"""
    """And return a response Header"""
    RETHEADER=''
    if Header=='':
        return ''
    else:
        dt=localtime()
        RETHEADER='HTTP/1.1 200 OK\nDate: ' + str(dt[2]) + ' ' +str(dt[1])+ ' ' + str(dt[0]) + ' ' +str(dt[3]) + ':' + str(dt[4]) + ':' + str(dt[5]) + "\n"
        RETHEADER=RETHEADER+'Server: ' + IDENT +'\n'
        if Header.find("Connection:")!=-1:
            RETHEADER=RETHEADER+'Connection: Keep-Alive\n'
            RETHEADER=RETHEADER+'Keep-Alive: timeout=5, max=100\n'
        return RETHEADER+"\n"
    
#####################################################

#####################################################

def EXECFILE(client,filename):
    """Execute a file and pass MSG to stdin, the response is sent to client"""
    try:
        if filename.find(".pl")!=-1 or filename.find(".py")!=-1 or filename.find(".cgi")!=-1 or filename.find(".sh")!=-1:
            fd=open(filename,"r")
            execpath=fd.readline()
            execpath=execpath[2:-1] #remove #! and \n
            execpath=execpath + " " + filename #WARNING
            filename=execpath
        if filename.find(";")!=-1 or filename.find("|")!=-1 or filename.find("$")!=-1 or filename.find("*")!=-1 or filename.find("@")!=-1 or filename.find("%")!=-1 or filename.find("&")!=-1 or filename.find("`")!=-1:
            return 1
        stdin, stdout = popen2.popen2(filename)
    except:
        return 1
    try:
        GOT=stdin.read()
        client.send(GOT)
        client.close()
    except:
        print "A fatal error ocurred when executing "+filename
        return 1
    return 0

#####################################################

#####################################################

def DOLIST_TO(client,msg):
    """Send a archive list to the client when he call a dir who end in /"""
    """Don't show nothing if"""
    try:
        bck=msg
        #HTML="Connection: close\nContent-type: text/html; charset=iso-8859-1\n\n"
        HTML='<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">'
        HTML=HTML+'<html><title>'+msg+'</title><body><h3>Index of '+msg+'</h3><hr><br><br>'
        msg=HTML_DIR+msg
        ret=os.access(msg,os.F_OK)
        if ret==True:
            LIST=os.listdir(msg)
            for x in range(0,len(LIST)):
                if LIST[x]=="NO_ACCESS":
                    DO404(client)
                    return -1;
                elif LIST[x]=="index.html" or LIST[x]=="index.htm":
                    SENDFILE(client,HTML_DIR+bck+LIST[x],'r',1)
                    close(client);
                    return 2;
                HTML=HTML+'<b><a href="'+bck+LIST[x]+'">'+LIST[x]+'</a><br>'
            #Send page...
            HTML=HTML+'<br><hr><br><center><h6>'+IDENT+'</h6></center></body></html>'
            client.send(HTML)
            client.close()
        else:
            404(client)
    except:
        client.close()        
            

#####################################################
            
#####################################################

def DO404(client):
    err=SENDFILE(client,HTML_DIR+'/404.html','r',1)
    if err==1:
        print 'Error! > No :' + HTML_DIR + '/404.html'
        LogCLient("You must create a 404.html file in "+HTML_DIR+"\r\n")
    client.close()

#####################################################
                
#####################################################

def REQUEST_HEAD(client,frstbff):
    """Make a HEAD request"""  
    msg=frstbff+client.recv(BUFF-4)
    os.environ["REQUEST_METHOD"]='HEAD'
    pos=msg.find("\n")
    msg=msg[5:pos]
    if msg.find('HTTP/')!=-1:
        pos=msg.find('HTTP/')
        pos=pos-1
        msg=msg[0:pos]
    #transversal
    if msg.find("..")!=-1:
        client.close()
        return 0
    if msg.find("cgi-bin")==1:
        #404 no support to EXEC insisde HEAD method
        DO404(client)
    else:
        try:
            if msg[-1:]=="/" and msg!="/":
                DOLIST_TO(client,msg)
                return 0;
            elif os.stat(HTML_DIR+msg)[6]==0:
                DOLIST_TO(client,msg+'/')
                return 0;
        except:
            pass
        if msg == "/":
            msg=BAAR
        if msg.find('.html')!=-1 or msg.find('.htm')!=-1:
            err=SENDFILE(client,HTML_DIR+msg,'r',1)
        elif msg.find('.txt')!=-1 or msg.find('.c')!=-1:
            err=SENDFILE(client,HTML_DIR+msg, 'r',1)
        else:
            err=SENDFILE(client,HTML_DIR+msg,'rb',0)  
        if err==1:
            err=SENDFILE(client,HTML_DIR+'/404.html','r',1)
            if err==1:
                DO404(client)
                return -1
    return 0
    


#####################################################
                
#####################################################


def REQUEST_GET(client,frstbff):
    """Make a GET request client is the client socket and frstbff the first 4 bytes"""  
    msg=frstbff+client.recv(BUFF-4)
    os.environ["REQUEST_METHOD"]='GET' #set environment
    pos=msg.find("\n")
    msg=msg[4:pos]
    if msg.find('HTTP/')!=-1:
        pos=msg.find('HTTP/')
        pos=pos-1
        REQUEST_HEADER=msg[pos:] #a try to respond to browser subrequests
        msg=msg[0:pos]
    #check for transversal directory
    if msg.find("..")!=-1:
        client.close()
        return 0
    if msg.find("cgi-bin")==1:
        if msg.find(';')!=-1:
            client.close()
            return 1
        #here msg looks something like /cgi-bin/....
        msg=msg[9:] #remove "/cgi-bin/" now we have scriptname.x?var1=value1&...
        if msg.find('?')!=-1:
            #we have arguments to the cgi script
            pos=msg.find('?')
            opt=msg[(pos+1):]
            msg=msg[0:pos]
            os.environ['QUERY_STRING']=opt
            #environment set..
            #print CGI_DIR+msg
            err = EXECFILE(client,CGI_DIR+msg)
            if err==1:    
                err=SENDFILE(client,HTML_DIR+'/404.html','r',1)
                if err==1:
                    print "Error! > No "+HTML_DIR+'/404.html'
            
        else:
            err = EXECFILE(client,CGI_DIR+msg)
            if err==1:    
                err=SENDFILE(client,HTML_DIR+'/404.html','r',1)
                if err==1:
                    print "Error! > No "+HTML_DIR+'/404.html'
                    LogClient("You must create a 404.html file in "+HTML_DIR+"\r\n")
    else:
        try:
            if msg[-1:]=="/" and msg!="/":
                DOLIST_TO(client,msg)
                return 0;
            elif os.stat(HTML_DIR+msg)[6]==0:
                DOLIST_TO(client,msg+'/')
                return 0;
        except:
            pass
        if msg.find('?')!=-1:
            pos=msg.find('?')
            opt=msg[(pos+1):]
            msg=msg[0:pos]
            os.environ['QUERY_STRING']=opt
                
        if msg == "/":
            msg=BAAR
        if msg[-5:]=='.html' or msg[-4:]=='.htm':
            err=SENDFILE2(client,HTML_DIR+msg,'r',1,REQUEST_HEADER)
        elif msg[-4:]=='.php' and PHP!='':
            try:
                os.stat(HTML_DIR+msg)
            except:
                DO404(client)
                return 1
            REQUEST_HEADER=Basic_Header(REQUEST_HEADER)
            TEXT=exec_php(HTML_DIR+msg)
            if TEXT==-1:
                DO404(client)
                return 1
            else:
                client.send(REQUEST_HEADER+TEXT)
                client.close()
                return 0
        elif msg[-4:]=='.txt' or msg[-2:]=='.c':
            err=SENDFILE2(client,HTML_DIR+msg, 'r',1,REQUEST_HEADER)
        else:
            err=SENDFILE2(client,HTML_DIR+msg,'rb',0,REQUEST_HEADER)  
        if err==1:
            err=SENDFILE2(client,HTML_DIR+'/404.html','r',1,REQUEST_HEADER)
            if err==1:
                print 'Error! > No :' + HTML_DIR + '/404.html'
                LogCLient("You must create a 404.html file in "+HTML_DIR+"\r\n")
    client.close()
    return 0
            
######################################################
            
######################################################

def exec_php(filename):
    os.environ["INCLUDE"]=filename
    if os.environ["REQUEST_METHOD"]=='GET':
        try:
            stdin, stdout = popen2.popen2(PHP+" "+PHPBINDER) #PHPBINDER WILL include(filename)
            stdout.close()
            GOT=stdin.read()
            return GOT
        except:
            print "PHP binder cannot be executed as :"+PHP+""+PHPBINDER;
            return -1
    else:
        return -1;

######################################################
            
######################################################

def REQUEST_POST(client,msg):
    os.environ["REQUEST_METHOD"]='POST' #set post
    while msg.find("\n")==-1:
        msg=msg+client.recv(1)
    finder=""
    while finder.find("Content-Length:")==-1:
        while finder.find('\n')==-1:
            finder=finder+client.recv(1)
        if finder.find("Content-Length:")==-1:
            finder=""
    finder=finder[16:-2] #remove \r\n and Content Lenght..
    next_read=int(finder)
    while finder.find('\n')==-1:
        finder=finder+client.recv(1)
    POST=client.recv(next_read)
    #remove trash from msg (POST /)
    msg=msg[5:]; #+7 or +8 to remove cgi-bin
    if msg.find("HTTP/")==-1:
        msg=msg[0:-2]
    else:
        msg=msg[0:msg.find('HTTP/')]
    if msg.find(';')!=-1:
        client.close()
        return 0
    if msg.find('..')!=-1:
        client.close()
        return 0
    msg=msg[:-1]
    if  msg[-4:]==".php":
        RETX=_exec_PHP(HTML_DIR+msg,POST)
        if RETX==-1: #error he receive a beautiful 404 msg
            DO404(client)
            return 1
        else:
            try:
                client.send(RETX)
                client.close() 
                return 0
            except:
                pass
                return 
    #Isn't a php, remove cgi-bin/ and continue
    if msg.find("cgi-bin")!=-1:
        msg=msg[9:]
        if  EXEC_ARGC(client,CGI_DIR+msg,POST)==0:
            return 0
        else:
            if SENDFILE(client,HTML_DIR+'/404.html','r',1)==1:
                print 'Error! > No :' + HTML_DIR + '/404.html'
                LogClient("You must create a 404.html file in "+HTML_DIR+"\r\n")
        return 0
    else:
        DO404(client)
        return 1
            
######################################################
            
######################################################

def EXEC_ARGC(client,filename,msg):
    """Execute a file and pass MSG to stdin, the response is sent to client"""
    try:
        if filename.find(".pl")!=-1 or filename.find(".py")!=-1 or filename.find(".cgi")!=-1 or filename.find(".sh")!=-1:
            fd=open(filename,"r")
            execpath=fd.readline()
            execpath=execpath[2:-1] #remove #! and \n
            execpath=execpath + " " + filename #WARNING
            filename=execpath
        if filename.find(";")!=-1 or filename.find("|")!=-1 or filename.find("$")!=-1 or filename.find("*")!=-1 or filename.find("@")!=-1 or filename.find("%")!=-1 or filename.find("&")!=-1 or filename.find("`")!=-1:
            return 1
        stdin, stdout = popen2.popen2(filename)
    except:
        return 1
    try:
#Remove # to debug
#        print "Passing>"+msg
        stdout.write(msg)
#        print "Passed"
        stdout.close()
#        print "Reading"
        GOT=stdin.read()
        client.send(GOT)
#        print "Send "+GOT
        client.close()
    except:
        print "A fatal error ocurred when executing "+filename
        return 1
    return 0

######################################################

######################################################

def _exec_PHP(PHP_FILE,MSG):
    os.environ["INCLUDE"]=PHP_FILE
    os.environ["QUERY_STRING"]=MSG
    try:
        stdin, stdout = popen2.popen2(PHP+" "+PHPBINDER)
    except:
        print "PHP binder cannot be executed as :"+PHP+""+PHPBINDER;
        return 1
    try:
        stdout.close()
        GOT=stdin.read()
        return GOT
    except:
        return -1
    return 0

######################################################

######################################################

#call main();
if __name__ == '__main__':
   main()
