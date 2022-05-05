#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 3 14:38:38 2020

Note: The integrate_cyf.pyx source file needs to be cythonized before running this script.

@author: Katherine Holcomb
"""

import integrate_cyf as icyf

print (icyf.integrate_f(1.0, 51.0, 1000))