+++
# Slider widget.
widget = "slider"  # See https://sourcethemes.com/academic/docs/page-builder/
headless = true  # This file represents a page section.
active = true  # Activate this widget? true/false
weight = 250  # Order that this section will appear.

# Slide interval.
# Use `false` to disable animation or enter a time in ms, e.g. `5000` (5s).
interval = false

# Slide height (optional).
# E.g. `500px` for 500 pixels or `calc(100vh - 70px)` for full screen.
height = ""
# Slides.
# Duplicate an `[[item]]` block to add more slides.
[[item]]
  title = "Contact Us"
  align = "center"  # Choose `center`, `left`, or `right`.

  # Overlay a color or image (optional).
  #   Deactivate an option by commenting out the line, prefixing it with `#`.
  overlay_color = "#232D4B"  # An HTML color value.
  overlay_img = "grounds_early_fall.jpg"  # Image path relative to your `static/img/` folder.
  overlay_filter = 0.5  # Darken the image. Value in range 0-1.

  # Call to action button (optional).
  #   Activate the button by specifying a URL and button label below.
  #   Deactivate by commenting out parameters, prefixing lines with `#`.
  cta_label = "Get Help"
  cta_url = "https://rc.virginia.edu/support"
  cta_icon_pack = "fas"
  #cta_icon = "graduation-cap"

[[item]]
  title = "Upcoming Presentations"
  align = "center"

  overlay_color = "#232D4B"  # An HTML color value.
  overlay_img = "learning-bg.jpg"  # Image path relative to your `static/img/` folder.
  overlay_filter = 0.5  # Darken the image. Value in range 0-1.

  cta_label = "Next Events"
  cta_url = "https://rc.virginia.edu/education/workshops/"

[[item]]
  title = "Visit Office Hours"
  #content = "I am left aligned :smile:"
  align = "center"

  overlay_color = "#232D4B"  # An HTML color value.
  overlay_img = "brooks_hall_spring_ss_02.jpg" 
  overlay_filter = 0.5  # Darken the image. Value in range 0-1.

  cta_label = "Office Hours"
  cta_url = "https://rc.virginia.edu/support/#office-hours"
  cta_icon_pack = "fas"
+++
