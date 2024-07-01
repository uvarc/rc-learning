#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov  3 22:20:19 2021

@author: khs3z
"""
import cv2
import matplotlib.pyplot as plt
import numpy as np
from skimage.segmentation import clear_border
from skimage import measure
import pandas as pd

image = cv2.imread('fluorescent-cells.png')
nuclei = np.array(image[:,:,0]) # get blue channel

# apply Gaussian filter to smoothen image, then apply Otsu threshold
blurred = cv2.GaussianBlur(nuclei, (3, 3), 0)
ret, thresh = cv2.threshold(blurred,0,255,cv2.THRESH_BINARY+cv2.THRESH_OTSU)

# remove small particles
kernel = np.ones((3,3),np.uint8)
opening = cv2.morphologyEx(thresh,cv2.MORPH_OPEN,kernel, iterations=7)

# remove image border touching objects
opening = clear_border(opening)

# sure background area
sure_bg = cv2.dilate(opening,kernel,iterations=10)

# find sure foreground area
dist_transform = cv2.distanceTransform(opening,cv2.DIST_L2,5)
ret, sure_fg = cv2.threshold(dist_transform,0.6*dist_transform.max(),255,0)

# sure_fg is float32, convert to uint8 and find unknown region
sure_fg = np.uint8(sure_fg)
unknown = cv2.subtract(sure_bg,sure_fg)

# label markers 
ret, markers = cv2.connectedComponents(sure_fg)

# add one to all labels so that sure background is not 0, but 1
markers = markers + 1

# mark the region of unknown with zero
markers[unknown==255] = 0
markers = cv2.watershed(image,markers)
image[markers == -1] = [0,255,255]


# compute image properties and return them as a pandas-compatible table
p = ['label', 'area', 'equivalent_diameter', 'mean_intensity', 'perimeter']
props = measure.regionprops_table(markers, nuclei, properties=p)
df = pd.DataFrame(props)

# print data to screen and save
print (df)
df.to_csv('nuclei-data.csv')

# save output images
cv2.imwrite('nuclei.png', nuclei)
cv2.imwrite('nuclei-opening.png', opening)
cv2.imwrite('nuclei-sure_bg.png', sure_bg)
# cv2.imwrite('nuclei-dist_transform.png', dist_transform)
cv2.imwrite('nuclei-sure_fg.png', sure_fg)
cv2.imwrite('nuclei-unknown.png', unknown)
#cv2.imwrite('nuclei-markers.png', markers)
cv2.imwrite('image-segmented.png', image)

for i,img in enumerate([image,nuclei,opening,sure_bg,sure_fg,dist_transform,unknown,markers,image]):
    plt.subplot(331+i).imshow(img, cmap='jet')

