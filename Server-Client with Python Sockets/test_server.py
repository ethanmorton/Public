import socket
from threading import Thread, Lock, active_count
from server_support import *
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

thread_dict = {}

go = True

def removekey(key):
    del thread_dict[key]

def send(conn, msg):
    try:
        message = msg.encode(FORMAT)
        msg_length = len(message)
        send_length = str(msg_length).encode(FORMAT)
        send_length += b' ' * (HEADER - len(send_length))
        conn.send(send_length)
        conn.send(message)
    except ValueError as e:
        print(e)

def handle_client(conn, addr):
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
        print("The connection was terminated by the connectee.")
    conn.close()


def start():
    print(f"Started server, listening on {ADDR}\n")
    server.listen()
    num = 0
    while go:
        conn, addr = server.accept()
        thread = Thread(target=handle_client, args=(conn, addr))
        msg_length = int(conn.recv(HEADER).decode(FORMAT))
        name = conn.recv(msg_length).decode(FORMAT)
        thread_dict[conn] = [addr, name]

        thread.start()
        print(f"Active connections: {active_count() - 3}")
        num += 1

class terminal(Cmd):
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
    term = terminal()
    term.cmdloop()
    print("Server input thread exited.")

print("Starting server...")
server_thread = Thread(target=start)
server_thread.start()

print("Starting server input thread...")
input_thread = Thread(target=server_send)
input_thread.start()