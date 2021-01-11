import socket

def send(serverMACAddress,port=3):
    """
    Entrée :    - hostMACAddress : (str)     adresse MAC de l'adaptateur du serveur
                - port : (int)              port (indiférent mais doit etre le même que celui du client)                 
    """
    s = socket.socket(socket.AF_BLUETOOTH, socket.SOCK_STREAM, socket.BTPROTO_RFCOMM)
    s.connect((serverMACAddress,port))

    flag=True
    while flag:
        text = input()
        if text == "quit":
            flag=False
        else:
            s.send(bytes(text, 'UTF-8'))
    s.close()