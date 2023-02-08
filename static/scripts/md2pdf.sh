#!/bin/bash
#
# Make sure that the only line in each file with the string weight: is 
# for weighting (ordering).  
#
# Also make sure that the weights are all distinct.
#
wd=`pwd`
folder=`basename $wd`
declare pdf_array
startline=`grep "weight:" _index.md`
start=${startline//"weight:"/}
weight_array=($start)
pdf_array[$start]=$folder".pdf"
chromium-browser --headless --disable-gpu --print-to-pdf=$folder.pdf "https://staging.learning.rc.virginia.edu/notes/"$folder
for file in `ls *md`; do
   if [[ "$file" == "_index.md" || "$file" == "index.md" ]]; then
       continue
   fi 
   base=${file%.md}
   keyline=`grep "weight:" $file`
   key=${keyline//"weight:"}
   pdf_file=$base".pdf"
   weight_array+=($key)
   pdf_array[$key]=$pdf_file

   url="https://staging.learning.rc.virginia.edu/notes/"${folder}"/"${base}
   echo $url
   chromium-browser --headless --quiet --disable-gpu --print-to-pdf=$pdf_file $url
done

readarray -t order < <(printf '%s\n' "${weight_array[@]}" | sort -b -g)

command="qpdf --empty --pages "

for wt in ${order[*]}; do
    command="$command ${pdf_array[$wt]}"
done
command=$command" -- notes.pdf"

$command

# Clean up
for file in ${pdf_array[@]}; do
    rm $file
done

if [[ ! -d pdf ]]; then
    mkdir pdf
fi
mv notes.pdf pdf
