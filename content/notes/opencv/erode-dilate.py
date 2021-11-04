#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov  3 20:37:52 2021

@author: khs3z
"""
import cv2
import numpy as np
import matplotlib.pyplot as plt

image = cv2.imread('morph-input.png',0)
# create square shaped 7x7 pixel kernel
kernel = np.ones((7,7),np.uint8)

# dilate, erode and save results
dilated = cv2.dilate(image,kernel,iterations = 1)
eroded = cv2.erode(image,kernel,iterations = 1)
opened = cv2.morphologyEx(image, cv2.MORPH_OPEN, kernel)
closed = cv2.morphologyEx(image, cv2.MORPH_CLOSE, kernel)
kernel = np.ones((2,2),np.uint8)
gradient = cv2.morphologyEx(image, cv2.MORPH_GRADIENT, kernel)

cv2.imwrite('morph-dilated.png', dilated)
cv2.imwrite('morph-eroded.png', eroded)
cv2.imwrite('morph-opened.png', opened)
cv2.imwrite('morph-closed.png', closed)
cv2.imwrite('morph-gradient.png', gradient)

# display results
_,ax = plt.subplots(1,6)
ax[0].imshow(image, cmap='gray')
ax[0].axis('off')
ax[1].imshow(dilated, cmap='gray')
ax[1].axis('off')
ax[2].imshow(eroded, cmap='gray')
ax[2].axis('off')
ax[3].imshow(opened, cmap='gray')
ax[3].axis('off')
ax[4].imshow(closed, cmap='gray')
ax[4].axis('off')
ax[5].imshow(gradient, cmap='gray')
ax[5].axis('off')
plt.show()
