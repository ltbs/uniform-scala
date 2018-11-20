"use strict";

/**
 * Ofsted reports cookie handling
 */
(function(OFSTED, $) {
  var ORCookies = OFSTED.ORCookies = {
    /**
     * The init function is called on page load.
     *
     * Add any methods in here that should be run on page load.
     * e.g the dropdowns util will automatically add an expand / collapse where needed.
     */
    init: function () {

      // TODO: Look into a proper way of handling
      //       and dealing with cookies, maybe in PHP?
      ORCookies.dissmissBanner();
    },

    /**
    * Method to create cookie.
    */
    createCookie: function(name, value, mins) {
      if (mins) {
        var date = new Date();

        date.setTime(date.getTime()+(mins*60*1000));
        var expires = "; expires="+date.toGMTString();
      } else {
        var expires = "";
      }

      // Not including expires to have cookie last as long as session in browser
      return document.cookie = name + '=' + value + '; path=/';
    },

    /**
    * Close button for cookie banner.
    */
    dissmissBanner: function() {
      if (document.cookie.indexOf("bannerDissmissed=") < 0) {
        $('.cookie-banner').show();
      }

      $('.cookie-banner__close').click(function(e) {
        $(this).closest('.cookie-banner').hide();
        // Create cookie for dismissing banner
        ORCookies.createCookie('bannerDissmissed', true, 1000);
        e.preventDefault();
      });
    },

  }
})(window.OFSTED, window.jQuery);
