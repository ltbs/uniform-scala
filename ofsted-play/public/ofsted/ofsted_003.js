"use strict";

/**
 * Commonly-use utilities for the Ofsted Reports site.
 *
 * Any utility functions or common methods, for example JS dropdowns, belong in this file.
 */
(function(OFSTED) {
  OFSTED.Utils = {
    MAPS_API_KEY : 'AIzaSyCkxZfeSRRP5Wlv4_BFhckfdBJ4G2T2mn8',
    /**
     * The init function is called on page load.
     *
     * Add any methods in here that should be run on page load.
     * e.g the dropdowns util will automatically add an expand / collapse where needed.
     */
    init: function() {
      console.log('=== [OFSTED UTILS] init ===');
      OFSTED.Utils.loadMap();
      OFSTED.Utils.expander();
      OFSTED.Utils.detailsToggle();
    },

    /**
     * Add the Google Maps Api script to the page. This is done via a script to avoid race conditions
     * where the 'initMap' function doesn't exist yet.
     */
    loadMap: function() {
      var tag = document.createElement('script');
      tag.src = 'https://maps.googleapis.com/maps/api/js?key=' + OFSTED.Utils.MAPS_API_KEY + '&callback=initMap&libraries=places';
      tag.setAttribute('defer', 'defer');
      tag.setAttribute('async', 'async');
      document.querySelector('head').appendChild(tag);
    },
    /**
     *
     * @param el - HTML element that we're looking for the height of.
     * @param includeMargin - whether or not to add the margins to the height.
     * @returns {number}
     */
    outerHeight: function(el, includeMargin) {
      var style = getComputedStyle(el),
        height = el.offsetHeight;

      if (includeMargin) {
        height += parseInt(style.marginTop) + parseInt(style.marginBottom);
      }

      return height;
    },

    /**
     *
     * Add a JS expand / collapse functionality to lists / blocks
     * Params are added to the HTML with `data-` attributes to allow differing params.
     * The wrapper should add the `js-expander` class to be picked up by the method.
     * The toggle target should be marked up with `.js-expander-toggle`
     * Data params are :
     *
     *   - data-target    -   CSS selector for the target. If not specified this defaults to `.js-dropdown-toggle`
     *   **** ONE OF THE BELOW ITEMS IS REQUIRED..  IF NOT NOTHING IS SELECTED THE HEIGHT WILL BE 0
     *   - data-max-items (optional)  For a list, show a specified number of items before collapsing the remainder of the list
     *   - data-max-height (optional) For a block, define a max-height before collapsing the rest of the block
     *
     *
     */
    expander : function() {
      console.log('=== [OFSTED UTILS] expander ===');
      // Get all the expanders on the page.
      var expanders = [].slice.call(document.querySelectorAll('.js-expander'));

      expanders.forEach(function(exp) {
        var maxItems = exp.getAttribute('data-max-items'),
          seeMoreText = exp.getAttribute('data-see-more'),
          maxHeight = 0,
          selector = exp.getAttribute('data-selector') || 'li',
          items = exp.querySelectorAll(selector),
          seeMoreLink = exp.getAttribute('data-target') || false,
          force = exp.getAttribute('data-force') || false,
          remainder = '';

        if (maxItems && items.length > maxItems) {
          for (var i = 0; i < maxItems; i++) {
            maxHeight += OFSTED.Utils.outerHeight(items[i], true);
          }
        }

        if ((maxHeight && exp.clientHeight > maxHeight) || force) {
          if (exp.getAttribute('data-count') && exp.getAttribute('data-count').toLowerCase() === 'true') {
            if(items.length > maxItems) {
              remainder = ' (' + (items.length - maxItems) + ')';
            }
          }

          exp.style.height = maxHeight + 'px';
          exp.style.overflow = 'hidden';

          if(!seeMoreLink) {
            seeMoreLink = document.createElement('span');
            seeMoreLink.className = 'js-see-more expander-see-more';
            seeMoreLink.setAttribute('aria-hidden', true);
            seeMoreLink.textContent = seeMoreText + remainder;
            exp.parentNode.insertBefore(seeMoreLink, exp.nextSibling);
          } else {
            seeMoreLink = exp.parentNode.querySelector(seeMoreLink);
          }

          seeMoreLink.addEventListener('click', function seeMoreClickHandler(e) {
            if (exp.style.height) {
              exp.style.height = '';
              exp.style.overflow = 'visible';


              // Set the text to be 'See fewer if it was initially set to say 'See more'
              // otherwise just set it to seeMoreText
              if (seeMoreText === 'See more') {
                seeMoreLink.textContent = 'See fewer';
              } else {
                seeMoreLink.textContent = seeMoreText;
              }
              seeMoreLink.classList.add('expander-open');
            } else {
              exp.style.height = maxHeight + 'px';
              exp.style.overflow = 'hidden';
              seeMoreLink.textContent = seeMoreText + remainder;
              seeMoreLink.classList.remove('expander-open');

            }
            $(window).trigger('optionselect.recalculate');

          });
        }
      });

      $(window).trigger('optionselect.recalculate');
    },

    detailsToggle: function() {
      // Get the details column.
      var detailsCol = document.querySelector('.details-column'),
        // And the header toggle
        detailsToggle = document.querySelector('.js-details-collapse');

      if(!detailsToggle) {
        return;
      }

      detailsToggle.addEventListener('click', function() {
        // Google maps will need to redraw on hide / show so here we will trigger a window resize event
        var event = document.createEvent('HTMLEvents');
        detailsToggle.classList.toggle('details-expanded');
        detailsCol.classList.toggle('details-visible');
        event.initEvent('resize', true, false);
        window.dispatchEvent(event);
        if(detailsCol.classList.contains('details-visible')) {
          $(window).trigger('showhide.show');
          detailsToggle.setAttribute('aria-expanded', 'true');
          detailsCol.setAttribute('aria-hidden', 'false');
        } else {
          detailsToggle.setAttribute('aria-expanded', 'false');
          detailsCol.setAttribute('aria-hidden', 'true');
        }

        // Here we'll want to recentre the map, too -- just in case
        if (window.CustomEvent) {
          var event = new CustomEvent('map.recentre', {});
        } else {
          var event = document.createEvent('CustomEvent');
          event.initCustomEvent('map.recentre', true, true, {});
        }

        window.dispatchEvent(event);
      });
    }

  };
})(window.OFSTED);
