#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov  3 14:02:54 2021

@author: khs3z
"""


import numpy as np
import cv2
from matplotlib import pyplot as plt

noisy = cv2.imread('clown-noisy.png')

# define denoising parameters
h = 15  
hColor = 15
templateWindowSize = 7
searchWindowSize = 21

# denoise and save
denoised = cv2.fastNlMeansDenoisingColored(noisy,None,h,hColor,templateWindowSize,searchWindowSize)
cv2.imwrite('clown-denoised.png', denoised)

# display
plt.subplot(121),plt.imshow(cv2.cvtColor(noisy, cv2.COLOR_BGR2RGB), interpolation=None)
plt.subplot(122),plt.imshow(cv2.cvtColor(denoised, cv2.COLOR_BGR2RGB), interpolation=None)
plt.show()


"""
import cv2
import numpy as np
import matplotlib.pyplot as plt

image = cv2.imread('clown.png')
# convert all to float64
gray = [np.float64(i) for i in image]

# create a noise of variance 25
noise = np.random.randn(*gray[1].shape)*10

# Add this noise to images
noisy = [i+noise for i in gray]

# Convert back to uint8
noisy = [np.uint8(np.clip(i,0,255)) for i in noisy]

# Denoise 3rd frame considering all the 5 frames
dst = cv2.fastNlMeansDenoisingMulti(noisy, 2, 5, None, 4, 7, 35)

plt.subplot(131),plt.imshow(gray[2],'gray')
plt.subplot(132),plt.imshow(noisy[2],'gray')
plt.subplot(133),plt.imshow(dst,'gray')
plt.show()
"""