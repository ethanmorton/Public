import socket
from threading import Thread, Lock, active_count
from cmd import Cmd

HEADER = 64
PORT = 5000
#SERVER = "192.168.0.253"
SERVER = socket.gethostbyname(socket.gethostname()) #get ip address by getting the name
ADDR = (SERVER, PORT) #Address
FORMAT = "utf-8"
DISCONNECT_MESSAGE = "!DISCONNECT"

print(socket.gethostname())
print(socket.gethostbyname((socket.gethostname())))

server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

server.bind(ADDR)

#thread dictionary needed because each connection runs on its own thread, and is difficult to access in order to send information independent of the recieving loop
thread_dict = {}

go = True

def removekey(key):
    '''
    for removing an entry from the thread dictionary
    '''
    del thread_dict[key]

def send(conn, msg):
    '''
    to send a message to the client

    first encodes the message, then sends a message with specified length HEADER that contains 
    the length of the next message, then send the next message

    Handles one exception, ValueError, in case the for some reason that flag is thrown
    '''
    try:
        msg = msg + "\n"
        message = msg.encode(FORMAT)
        msg_length = len(message)
        send_length = str(msg_length).encode(FORMAT)
        send_length += b' ' * (HEADER - len(send_length))
        conn.send(send_length)
        conn.send(message)
    except ValueError as e:
        print(e)

def handle_client(conn, addr):
    '''
    the standard function for each connection from a client

    conn - the connection object that is returned from a successful connection
    addr - the ip and port the connection is connecting from, in a tuple :: (IP, PORT)

    runs a while loop until a specific message, the DISCONNECT_MESSAGE is sent from the client, at which point the loop wil and and the connection will terminate

    meant to specifically recieve and decode messages from the client side
    '''
    try:
        print(f"New connection.\n{addr} connected with alias {thread_dict[conn][1]}.")
        connected = True
        while connected:
            try:
                msg_length = conn.recv(HEADER).decode(FORMAT)
                if msg_length:
                    msg = conn.recv(int(msg_length)).decode(FORMAT)
                    print(f"{addr} with msg:\n {msg}\n")
                    send(conn, "Message recieved")
                    if msg == DISCONNECT_MESSAGE:
                        connected = False
                        print(f"Client {addr} disconnected")
                        send(conn, "Disconnecting from server...")
                        removekey(conn)
            except UnicodeDecodeError as e:
                print("There was a problem decoding the message.")
            except ValueError as v:
                print("There was a problem determining the length of the message.")
    except ConnectionResetError as e:
        print(f"{thread_dict[conn][1]} has disconnected unexpectedly")
        print("The connection was terminated by the connectee.")
        removekey(conn)
    conn.close()


def start():
    '''
    the start function is the main loop of the server. it listens for a 
    connection until the go variable is False(currently not implmented)

    every time a connection is made, the start function adds the relevant 
    data to the thread_dict as well as accepting the first transmission, 
    which is the name, and then starts a thread to handle that connection

    handles an unesxpected disconnection from client side
    '''
    print(f"Started server, listening on {ADDR}\n")
    server.listen()
    while go:
        try:
            conn, addr = server.accept()
            thread = Thread(target=handle_client, args=(conn, addr))
            msg_length = int(conn.recv(HEADER).decode(FORMAT))
            name = conn.recv(msg_length).decode(FORMAT)
            thread_dict[conn] = [addr, name]
            send(conn, "Connected to server")

            thread.start()
            print(f"Active connections: {active_count() - 3}")
        except ConnectionResetError as e:
            print(f"connection :: ", conn, "\nwith address :: ", addr, "\n terminated forcibly by the client side")

class terminal(Cmd):
    '''
    this class is intended for displaying information and allowing the server side user to directly interact with clients and data

    it creates an interactive terminal in the comand prompt that this script is run in
    '''
    prompt = '<Server> : '
    intro = "Server input thread has started. '?' or 'help' for a list of commands."
    def do_exit(self, inp):
        print("Bye!")
        return True
    
    def help_exit(self):
        print("Usage :: exit")
        print("Exits the running console.")
    
    def do_send(self, inp):
        if active_count() > 3:
            name = input("Please enter the name of the person you wish to send a message to:\n")
            for key, value in thread_dict.items():
                if value[1] == name:
                    send(key, "Server <::> " + input("Enter Your message now:\n"))
        else:
            print("There are currently no active connections.")
    
    def help_send(self):
        print("Usage :: send [message]")
        print("sends a message to the client of your choosing")
    
    def do_listcon(self, inp):
        if active_count() <= 3:
            print("There are no active connections")
        for key, value in thread_dict.items():
            print(f"Connection from {value[0]}, with name {value[1]}")
    
    def help_listcon(self):
        print("Usage :: listcon")
        print("prints the current connections to their console, and associated names")

def server_send():
    '''
    the function for using the cmd terminal in a separate thread
    '''
    term = terminal()
    term.cmdloop()
    print("Server input thread exited.")

print("Starting server...")
server_thread = Thread(target=start)
server_thread.start()

print("Starting server input thread...")
input_thread = Thread(target=server_send)
input_thread.start()