import os
import string
from datetime import datetime, timezone

base = os.getcwd().split('/')[-1]

with open('out.md') as f:
    lines = f.readlines()

sections = []
current_section = []
main_title = None
inside_code_block = False  # Flag to track if we're inside a code block

for line in lines:
    # Check if entering or exiting a code block
    if line.strip().startswith("```"):
        inside_code_block = not inside_code_block
        current_section.append(line)
        continue

    # Get rid of backslashes before punctuations
    line_list = []
    for i, char in enumerate(line):
        if i < (len(line) - 1) and (char == '\\' and line[i + 1] in string.punctuation):
            pass
        else:
            line_list.append(char)

    newline = "".join(line_list)

    # Replace Markdown with Hugo figure shortcode
    if "![]" in newline and not inside_code_block:
        newline2 = newline.replace('![](img/', '')
        line_end = newline2.find(')')
        image = newline2[:line_end]
        newline = "{{< figure src=/notes/" + base + "/img/" + image + " >}}\n"

    if line.startswith('#') and not inside_code_block:
        header_level = line.count('#', 0, len(line.split(' ')[0]))
        title = line.lstrip('#').strip()

        if header_level == 1:  # Main title
            if main_title is not None:  # Save the previous section
                sections.append((main_title, current_section))
            main_title = title
            current_section = []
        elif header_level in {2, 3}:  # Subtitles
            current_section.append(line)  # Append directly in order
        else:
            current_section.append(newline)
    else:
        current_section.append(newline)

# Append the last section
if main_title is not None:
    sections.append((main_title, current_section))

# Now set up the files

date = datetime.now(timezone.utc).strftime('%Y-%m-%d-%H:%M:%SZ')

# Index
with open('_index.md', 'w') as nfile:
    nfile.write('---' + "\n")
    nfile.write('title: ' + base + "\n")
    nfile.write('date: ' + date + "\n")
    nfile.write('authors: ' + "[uvarc]\n")
    nfile.write('categories: ' + "[Fill]\n")
    nfile.write('tags: ' + "[fill]\n")
    nfile.write('type: docs ' + "\n")
    nfile.write('weight: 1 ' + "\n")
    nfile.write("\n")
    nfile.write("menu: \n")
    nfile.write("    " + base + ":\n")
    nfile.write('---' + "\n")
    nfile.write("\n")
    nfile.write("Title of Session\n")

for n, (title, content) in enumerate(sections):
    weight = (n + 1) * 50
    with open(f"{base}_{n}.md", 'w') as nfile:
        nfile.write('---' + "\n")
        nfile.write('title: ' + title + "\n")
        nfile.write('date: ' + date + "\n")
        nfile.write('type: docs ' + "\n")
        nfile.write('weight: ' + str(weight) + "\n")
        nfile.write("menu: \n")
        nfile.write("    " + base + ":\n")
        nfile.write('---' + "\n")
        nfile.write("\n")
        nfile.write("".join(content))
