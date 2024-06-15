+++
title= "Tutorials"
subtitle=""
date= "2020-05-13T00:00:00"  # Add today's date.
type = "widget_page"  # Page type is a Widget Page
widget = "blank"  
active= true
headless = false  # This file represents a page section.

# ... Put Your Section Options Here (title etc.) ...

[content]
  # Page type to display. E.g. post, talk, or publication.
  page_type = "talk"
  
  # Choose how much pages you would like to display (0 = all pages)
  count = 0
  
  # Choose how many pages you would like to offset by
  offset = 0

  # Page order. Descending (desc) or ascending (asc) date.
  order = "desc"

  # Filter posts by a taxonomy term.
  [content.filters]
    tag = ""
    category = ""
    publication_type = ""
    exclude_featured = false
    exclude_past = false
    exclude_future = false
    
[design]
  # Toggle between the various page layout types.
  #   1 = List
  #   2 = Compact
  #   3 = Card
  #   4 = Citation (publication only)
  view = 2

# Optional header image (relative to `static/img/` folder).
#header:
#  caption: ""
#  image: ""
+++
