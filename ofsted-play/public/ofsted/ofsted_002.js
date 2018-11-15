"use strict";

/**
 * Helpers for the Ofsted Search page
 */
(function(OFSTED, $) {
  var ORAC = OFSTED.Autocomplete = {
    /**
     * The init function is called on page load.
     *
     * Add any methods in here that should be run on page load.
     * e.g the dropdowns util will automatically add an expand / collapse where needed.
     */
    init: function () {
      console.log('=== [OFSTED SEARCH] init ===');
      // Check to see if there's an initMap funciton already defined; if not we'll just fire an init event for us.
      if(typeof(window.initMap) === 'undefined') {
        window.initMap = function() {
          $(window).trigger('OR.ACInit');
        };
      }

      $(window).on('OR.ACInit', ORAC.locationAC);


      // Accessible GOV.uk autocomplete
      var autocompletes = [].slice.call(document.querySelectorAll('.js-autocomplete'));
      if(autocompletes.length) {
        ORAC.setupAutocomplete(autocompletes);
      }
    },

    /**
     * Set up an autocomplete API instance for location search
     */
    locationAC: function() {
      // Options for the autocomplete widget.
      var options = {
            componentRestrictions: {
              country: ['gb']
            },
            // types: ['(regions)']
            types: ['geocode']
          },
          autocomplete,
          latField = document.querySelector('#place_lat'),
          lngField = document.querySelector('#place_lng'),
          radiusField = document.querySelector('#place_radius'),
          searchField = document.querySelector('#search_location');

      if(searchField) {
        autocomplete = new google.maps.places.Autocomplete(searchField, options);
        autocomplete.addListener('place_changed', function(e) {
          if(latField) {
            latField.value = '';
          }
          if (lngField) {
            lngField.value = '';
          }
          if (radiusField) {
            radiusField.value = '';
          }

          // Get the place object from Google.
          var place = autocomplete.getPlace(),
              lat = (place && place.geometry) ? place.geometry.location.lat() : '',
              lng = (place && place.geometry) ? place.geometry.location.lng() : '',
              views = (place && place.geometry) ? place.geometry.viewport : '';
          if(latField) {
            latField.value = lat;
          }
          if (lngField) {
            lngField.value = lng;
          }
          if (radiusField) {
            if(views) {
              var swDistance = ORAC.calculateLatLongDistance(lat, lng, views.f.b, views.b.b);
              var neDistance = ORAC.calculateLatLongDistance(lat, lng, views.f.f, views.b.f);
              radiusField.value = Math.max(swDistance, neDistance);
            }
          }

          $(searchField).focus();gs
        });

        $(searchField).on('change', function() {
          if(this.value.length < 1) {
            if(latField) {
              latField.value = '';
            }

            if(lngField) {
              lngField.value = '';
            }

            if(radiusField) {
              radiusField.value = '';
            }
          }
        });
      }
    },

    calculateLatLongDistance: function(lat1, lon1, lat2, lon2) {
      var dLat = (lat1 - lat2) * (Math.PI / 180);
      var dLon = (lon1 - lon2) * (Math.PI / 180);
      var a =
          Math.sin(dLat / 2) * Math.sin(dLat / 2) +
          Math.cos(lat2 * (Math.PI / 180)) * Math.cos(lat1 * (Math.PI / 180)) *
          Math.sin(dLon / 2) * Math.sin(dLon / 2)
      ;
      var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
      return Math.round(6371 * c * 0.62);
    },

    /**
     * For each autocomplete item on the page, set up a new instance of the GOV.uk Accessible Autocomplete
     * @param ac   {Array}    all the autocomplete items.
     */
    setupAutocomplete: function(autocompletes) {
      autocompletes.forEach(function(ac) {
        var select = ac.querySelector('select'),
            wrapper = $(select).closest('.govuk-option-select');

        if (select && typeof(accessibleAutocomplete) !== 'undefined') {
          accessibleAutocomplete.enhanceSelectElement({
            selectElement: select,
            defaultValue: '',
            minLength: 3
          });
        }
      });

    }
  }
})(window.OFSTED, window.jQuery);
