import string
import random

def scramble_word(word):
   if any(word.startswith(s) for s in string.punctuation):
      start=2
   else:
      start=1
   if any(word.endswith(s) for s in string.punctuation):
      end=-2
   else:
      end=-1
   letters=list(word)
   middle=letters[start:end]
   random.shuffle(middle)
   mid=''.join(middle)
   if len(word)<3:
      scramble=word
   else:
      scramble=word[:start]+mid+word[end:]
   return scramble

def scramble_line(line):
   newword_list=[]
   wordlist=line.split()
   for word in wordlist:
      newword_list.append(scramble_word(word))
   newline=' '.join(newword_list)
   return newline

def scramble_text(text):
   lines=text.splitlines()
   newtext_list=[]
   for line in lines:
      newtext_list.append(scramble_line(line))
   newtext='\n'.join(newtext_list)
   return newtext

def main():
   
   while True:
      filename=raw_input("Enter the file name:")
      if (filename):
         break
      else:
         print "No file specified, please try again."
         continue

   with open(filename,'rU') as fin:
      text=fin.read()

   with open("Scrambled_"+filename,'w') as fout:
      fout.write(scramble_text(text))

if __name__=='__main__':
   main()
