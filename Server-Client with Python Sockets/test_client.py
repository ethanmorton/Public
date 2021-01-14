import socket
from threading import Thread
from cmd import Cmd

HEADER = 64
PORT = 5000
#SERVER = "ec2-3-16-91-182.us-east-2.compute.amazonaws.com"
SERVER = "192.168.0.253"
FORMAT = "utf-8"
DISCONNECT_MESSAGE = "!DISCONNECT"
ADDR = (SERVER, PORT)
SELF = socket.gethostbyname(socket.gethostname()) #get self ip by name

client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
client.connect(ADDR)

def send(msg):
    '''
    sending a message to the server

    the client socket is global in this case, and so it can be easily accessed
    the mesage is sent by sending a message of length HEADER containing the 
    length of the next message, then sending the next message

    it handles two types of errors, an encoding problem, where the message may be corrupted or have unknown characters,
    and a value error, where the conversion to string might have had a problem
    '''
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
    '''
    for recieving a message from the server

    it recieves a message of length HEADER containing the length of the next mesage, then recieves a message

    it does not currently handle decoding errors
    '''
    print("Connected to server")
    msg_length = client.recv(HEADER).decode(FORMAT)
    msg = client.recv(int(msg_length)).decode(FORMAT)
    print(msg, "\n")

def start():
    '''
    the start function us the main client side loop, it asks for a name, then sends it to the 
    server, and begins the loop of asking if a message should be sent. if declined, it will 
    send the DISCONNECT_MESSAGE and he server will close the connection.

    it does not currently handle any errors
    '''
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