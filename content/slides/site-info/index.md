---
title: The rc-learning site
summary: An introduction maintaining the site.
authors: []
tags: []
categories: []
date: "2022-11-03T00:00:00Z"
slides:
  # Choose a theme from https://github.com/hakimel/reveal.js#theming
  theme: white
  # Choose a code highlighting style (if highlighting enabled in `params.toml`)
  #   Light style: github. Dark style: dracula (default).
  highlight_style: github
---

# Basic Info

[README](https://github.com/uvarc/rc-learning)

---

## Workflow

- Same as for the main rc.virginia.edu site
- Always check out and modify staging
- Clone into an appropriate directory, then cd to it and run /wherever/hugo server.  Example
  - (I don't know how to do this on Windows)
- website
  - rc-learning
  - rc-website
  - hugo (copy of hugo-extended)

---

## Theme

- Formerly called Academic, now called [Wowchemy](https://wowchemy.com/)
- Do not modify anything under the themes subdirectory
- Make changes into the corresponding folder under the top level
  - e.g. layouts/partials/page-footer.html modified from themes/academic/layout/partials/page_footer.html
- Hugo looks in top level first, then goes to theme 

---

## Code Highlighting

Code block
```python
porridge = "blueberry"
if porridge == "blueberry":
    print("Eating...")
```

---

## Math

In-line math: $x + y = z$

Block math:

$$
f\left( x \right) = \;\frac{{2\left( {x + 4} \right)\left( {x - 4} \right)}}{{\left( {x + 4} \right)\left( {x + 1} \right)}}
$$

---

## Diagrams

- Mermaid (https://mermaid-js.github.io/mermaid/#/)
- Example

```markdown
{{< diagram >}}
flowchart LR
   subgraph Users
       A(User) --> F((Internet))
       B(User) --> F
       C(User) --> F
    end
    subgraph Cluster
       F --> G(Frontend)
       G --> H{{Interconnection Network}}
       H --> K(Node)
       H --> L(Node)
       H --> M(Node)
       H --> N(Node)
       H --> S[(Storage)]
    end
{{< /diagram >}}
```

---

## Mermaid

View example [here](learning.rc.virginia.edu/courses/parallel_computing_introduction/parallel_hardware/)

---

##  Shortcodes

- code.html
  - shows code from a file
- code-snippet.html
  - copies a bit of code to user's clipboard 
- code-download
  - allows user to download a file containing code

---

## Site Design

- Three categories
  - short courses
  - tutorials
  - workshops
- probably should reorganize but we'll deal with that later

---

## Tutorials and Workshops

- Basically the same thing (hence the need to reorganize)
- Have a "landing page"
- Landing page has options for links/downloads (in ovals)
 - notes
 - slides
 - pdf

---

## Front Matter for Tutorials and Workshops "Landings"

- notes
 - directs to content/notes/samename
- slides
 - directs to content/slides/samename
 - expects Reveal.js slides
- url_slides
 - full (relative to Hugo paths or full URL) URL must be provided, e.g.
   - url_slides: /slides/r-intro-r-intro-slides.pdf
- pdf
   - expects to find a pdf in the same directory
- url_pdf
   - similar to url_slides
- url_dataset
   - data/whatever
   - or longer URL
- url_code
   - like url_dataset
---

## Short courses

- For longer material
- No "landing page"
- Be sure to include tags and categories

---

## Images

- Can be in /static/images if used more than once
- Can be local within course directory
- Reference like
  - {{< figure src="/courses/parallel_computing_introduction/img/SMP.png" caption="Schematic of an SMP system." >}}
  - figure shortcode is a Hugo built-in

---

## Document Types

- article
  - Can have a menu layout with "subchapters"
  - Chapter parent menus must be a file that isn't empty (a Hugo thing)
- book
  - no subchapters

---

## Aesthetics

- The title in the frontmatter will be Header 1
- I tend to prefer to avoid Header 1 subsequently unless you have only one file
