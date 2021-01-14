import socket
from threading import Thread
from cmd import Cmd

HEADER = 64
PORT = 5000
#SERVER = "ec2-18-220-80-245.us-east-2.compute.amazonaws.com"
SERVER = "192.168.0.253"
FORMAT = "utf-8"
DISCONNECT_MESSAGE = "!DISCONNECT"
ADDR = (SERVER, PORT)
SELF = socket.gethostbyname(socket.gethostname()) #get self ip by name

client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
client.connect(ADDR)

def send(msg):
    try:
        message = msg.encode(FORMAT)
        msg_length = len(message)
        send_length = str(msg_length).encode(FORMAT)
        send_length += b' ' * (HEADER - len(send_length))
        client.send(send_length)
        client.send(message)
    except UnicodeEncodeError:
        print("There was a problem encoding your message, please try again")
    except ValueError:
        print("Your message contains unaccepted values, please try again.")

def recieve():
    print("Connected to server")
    msg_length = client.recv(HEADER).decode(FORMAT)
    msg = client.recv(int(msg_length)).decode(FORMAT)
    print(msg, "\n")

def start():
    #"sign in"
    name = input("Please enter a name to be referred to:\n")
    send(name)

    while True:
        inp = input("Would you like to send a message?\n")
        if inp == "Y":
            send(input("Please type it below.\n"))
        elif inp == "N":
            print("OK")
            send("!DISCONNECT")
            print("Disconnected")
            break
        else:
            print("I didn't quite catch that.")


recv = Thread(target=recieve)
recv.start()

client_thread = Thread(target=start())
client_thread.start()