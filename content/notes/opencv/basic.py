#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov  2 22:14:24 2021

@author: khs3z
"""
import cv2

image = cv2.imread("clown.png", cv2.IMREAD_UNCHANGED)
(h, w, d) = image.shape
print("width={}, height={}, depth={}".format(w, h, d))

(b, g, r) = image[100, 50]
print("red={}, green={}, blue={}".format(r, g, b))

resized = cv2.resize(image,(500,500))
cv2.imshow('Resized', resized)

width = image.shape[0]
height = image.shape[1]
aspect = width/height
new_width = 500
new_height = int(new_width * aspect)
resized2 = cv2.resize(image,(new_width,new_height))
#cv2.imshow('Resized proportional', resized2)
#cv2.waitKey(0)

(B, G, R) = cv2.split(image)

cv2.imshow('Red', R)
cv2.imshow('Green', G)
cv2.imshow('Blue', B)

r = image.copy()
# set blue and green channels to 0
r[:, :, 0] = 0
r[:, :, 1] = 0
cv2.imshow('Red (RGB)', r)

import numpy as np
zeros = np.zeros((image.shape[0], image.shape[1]), dtype=np.uint8)
print (B.shape, zeros.shape)
merged = cv2.merge([B, G, zeros])
cv2.imshow("Merged", merged)
cv2.waitKey(0)
cv2.destroyAllWindows()