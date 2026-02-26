// Script for setting the alt text of the images in the fancybox lightbox.

// Wait for jQuery to be available before executing
(function() {
  function initFancyboxAltText() {
    if (typeof jQuery === 'undefined') {
      // jQuery not ready yet, try again
      setTimeout(initFancyboxAltText, 50);
      return;
    }

    (function ($) {
      // Use global Fancybox event so this runs even if the theme initialized Fancybox already.
      $(document).on('afterShow.fb', function (e, instance, slide) {
        try {
          // trigger element (anchor) that opened the slide
          var $trigger = slide && slide.opts && slide.opts.$orig ? slide.opts.$orig : null;
          var alt = '';

          if ($trigger && $trigger.length) {
            alt = $trigger.data('alt') || '';
          }

          // set alt on the image inside the slide content
          if (alt && slide && slide.$content) {
            slide.$content.find('img').attr('alt', alt);
          }
        } catch (err) {
          console.error('fancybox-alt-text error', err);
        }
      });
    })(jQuery);
  }

  // Start initialization
  initFancyboxAltText();
})();
