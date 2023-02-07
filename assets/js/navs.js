function

    $('.previous-page').click(function (event) {
      event.preventDefault();
      $('html, body').animate({
        'scrollTop': 0
      }, 800, function () {
        window.location.hash = "";
      });
    });

    $('.next-page').click(function (event) {
      event.preventDefault();
      $('html, body').animate({
        'scrollTop': 0
      }, 800, function () {
        window.location.hash = "";
      });
    });
