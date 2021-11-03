#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov  2 22:14:24 2021

@author: khs3z
"""
import cv2
import matplotlib.pyplot as plt

# Load image
image = cv2.imread("clown.png", cv2.IMREAD_UNCHANGED)
# get image dimensions, note specific order
h,w,d = image.shape
print("width={}, height={}, depth={}".format(w, h, d))
# get blue,green,red intensity at (100,50)
b,g,r = image[100, 50]
print("red={}, green={}, blue={}".format(r, g, b))
plt.imshow(cv2.cvtColor(image, cv2.COLOR_BGR2RGB))

# Resize image
image = cv2.imread("clown.png", cv2.IMREAD_UNCHANGED)
resized = cv2.resize(image,(500,500))

# resize width while preserving height proportions
width = image.shape[0]
height = image.shape[1]
aspect = width/height
new_width = 500
new_height = int(new_width * aspect)
resized2 = cv2.resize(image,(new_width,new_height))

# display the two resized images
_,ax = plt.subplots(1,2)
ax[0].imshow(cv2.cvtColor(resized, cv2.COLOR_BGR2RGB))
ax[0].axis('off')
ax[1].imshow(cv2.cvtColor(resized2, cv2.COLOR_BGR2RGB))
ax[1].axis('off')


# Split color channels
(B, G, R) = cv2.split(image)
# create 2x2 grid for displaying images
_, axarr = plt.subplots(2,2)
axarr[0,0].imshow(R, cmap='gray')
axarr[0,0].axis('off')
axarr[0,0].set_title('red')

axarr[0,1].imshow(G, cmap='gray')
axarr[0,1].axis('off')
axarr[0,1].set_title('green')

axarr[1,0].imshow(B, cmap='gray')
axarr[1,0].axis('off')
axarr[1,0].set_title('blue')

axarr[1,1].imshow(cv2.cvtColor(image, cv2.COLOR_BGR2RGB))
axarr[1,1].axis('off')
axarr[1,1].set_title('RGB')

    
import numpy as np
zeros = np.zeros((image.shape[0], image.shape[1]), dtype=np.uint8)
print (B.shape, zeros.shape)
merged = cv2.merge([B, G, zeros])
_,ax = plt.subplots(1,1)
ax.imshow(cv2.cvtColor(merged, cv2.COLOR_BGR2RGB))
ax.axis('off')
#plt.imshow(merged)
#cv2.imshow("Merged", merged)
#cv2.waitKey(0)
#cv2.destroyAllWindows()