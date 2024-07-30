import sys
import os
import string
from datetime import datetime, timezone

base=os.getcwd().split('/')[-1]

with open('out.md') as f:
    lines=f.readlines()

sections=[]
section=[]
for line in lines:
    #get rid of backslashes before punctuations
    line_list=[]
    for i,char in enumerate(line):
        if i<(len(line)-1) and (char=='\\' and line[i+1] in string.punctuation):
            pass
        else:
            line_list.append(char)

    newline="".join(line_list)
    #replace Markdown with Hugo figure shortcode, sort of
    if "![]" in newline:
        newline2=newline.replace('![](img/','')
        line_end=newline2.find(')')
        image=newline2[:line_end]
        newline="{{< figure src=/tutorials/"+base+"/img/"+image+" >}}\n"
    if line.startswith('#'):
        sections.append(section)
        #start over
        section=[newline]
    else:
        section.append(newline)

#Now set up the files

date=datetime.now(timezone.utc).strftime('%Y-%m-%d-%H:%M:%SZ')

#Index
with open('_index.md','w') as nfile:
    nfile.write('---'+"\n")
    nfile.write('title: '+base+"\n")
    nfile.write('date: '+date+"\n")
    nfile.write('authors: '+"[uvarc]\n")
    nfile.write('categories: '+"[Fill]\n")
    nfile.write('tags: '+"[fill]\n")
    nfile.write('type: docs '+"\n")
    nfile.write('weight: 1 '+"\n")
    nfile.write('date: '+date+"\n")
    nfile.write("\n")
    nfile.write("menu: \n")
    nfile.write("    "+base+":\n")    
    nfile.write('---'+"\n")
    nfile.write("\n")
    nfile.write("Title of Session\n")

for n,slide in enumerate(sections):
    if slide==[]: continue
    if slide[0].startswith('#'):
        title=slide[0][1:].strip()
        if ':' in title:
            title=title.replace(':','')
        del slide[0]
    else:
        title="Needs a Title"
    #space out
    weight=(n+1)*50
    with open(base+"_"+str(n)+".md",'w') as nfile:
        nfile.write('---'+"\n")
        nfile.write('title: '+title+"\n")
        nfile.write('date: '+date+"\n")
        nfile.write('type: docs '+"\n")
        nfile.write('weight: '+str(weight)+"\n")
        nfile.write("menu: \n")
        nfile.write("    "+base+":\n")    
        nfile.write('---'+"\n")
        nfile.write("\n")
        nfile.write("".join(slide))
