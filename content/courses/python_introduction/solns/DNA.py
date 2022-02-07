"""
  This program reads a gene file and creates a 
  dictionary of bases with the count as the value.

  Author:    A. Programmer
"""

bases='ATCG'

def countBases(DNA):
    DNAcounts={'A':0,'T':0,'C':0,'G':0}
    for base in DNA:
        if base in bases:
            DNAcounts[base]+=1
    return DNAcounts

def printBaseComposition(DNAcounts):
     total=float(DNAcounts['A']+DNAcounts['T']+DNAcounts['C']+DNAcounts['G'])

     for base in bases:
         print "%s:%.4f" % (base,DNAcounts[base]/total)


#In a real code you should read the name of the file from the command
#line (using sys.argv) or ask the user for the name.
infile="Homo_sapiens-APC.txt"
fin=open(infile,'r')

DNA=fin.read().strip("\n\r")

DNAcounts=countBases(DNA)
printBaseComposition(DNAcounts)
