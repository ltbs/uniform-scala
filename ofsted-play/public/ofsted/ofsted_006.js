// Always enable strict mode in your JS files.
"use strict";
// window.OFSTED = (typeof window.OFSTED !== 'undefined' && typeof window.OFSTED === 'object') ? window.OFSTED : {};
window.OFSTED = {};
/**
 * Core bootstrapping functionality for the Ofsted application.
 *
 * This will set up a framework to allow future plugins to be autoloaded if the script is included in the bundle
 * on the page.
 */
(function(OFSTED) {
OFSTED.Core = {
  /**
   * Bootstrap the core application.
   * All submodules will be check for an 'init' method, and if it exists it will be invoked automatically.
   *
   */
  bootstrap: function () {
    for (var m in OFSTED) {
      if (typeof(OFSTED[m].init) !== 'undefined' && typeof(OFSTED[m].init) === 'function') {
        OFSTED[m].init();
      }
    }
  },

};



})(window.OFSTED);
window.addEventListener('DOMContentLoaded', window.OFSTED.Core.bootstrap, false);
