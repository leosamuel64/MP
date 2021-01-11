import cv2
import imutils
import os

orangeLower = (5, 140, 185)
orangeUpper = (30, 215, 255)


camera = cv2.VideoCapture(0)


while True:

    BALLE = False

    (grabbed, frame) = camera.read()

    frame = imutils.resize(frame, width=600)
    hsv = cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)
    mask = cv2.inRange(hsv, orangeLower, orangeUpper)
    #mask = cv2.erode(mask, None, iterations=2)
    #mask = cv2.dilate(mask, None, iterations=2)

    cnts = cv2.findContours(mask.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)[-2]
    center = None

    if len(cnts) > 0:
        c = max(cnts, key=cv2.contourArea)
        ((x, y), radius) = cv2.minEnclosingCircle(c)

        if radius > 10:
            cv2.circle(frame, (int(x), int(y)), int(radius), (0, 255, 255), 2)
            u=os.system('clear')
            #print("x = "+str(int(x)))
            #print("y = "+str(int(y)))
            print("Radius  = "+str(radius))
            pos = (300-int(x))*-1    # position par rapport au centre de l'image x=300
            print("Pos = "+str(pos))
            BALLE = True
            diametre = radius*2
        else:
            u=os.system('clear')
            print("PAS DE BALLE")
            radius = -1
            x = -1
            y = -1
            BALLE = False
    else:
        u=os.system('clear')
        print("PAS DE BALLE")
        radius = -1
        x = -1
        y = -1
        BALLE = False

    cv2.imshow("Frame", frame)
    #cv2.imshow("Mask", mask)


camera.release()
cv2.destroyAllWindows()