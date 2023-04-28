"""
    Example of a few string functions/methods
"""

str="1,ninety,23.8,4,two"

split_str=str.split(',')

words=[]
for item in split_str:
    if item.isalpha():
        words.append(item)

written_number='-'.join(words)

print(written_number)
