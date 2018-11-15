(function ($) {
  "use strict";
  window.GOVUK = window.GOVUK || {};

  function OptionSelect(options){
    /* This JavaScript provides two functional enhancements to option-select components:
     1) A count that shows how many results have been checked in the option-container
     2) Open/closing of the list of checkboxes - this is not provided for ie6 and 7 as the performance is too janky.
     */

    this.$optionSelect = options.$el;
    this.$options = this.$optionSelect.find("input[type='checkbox']");
    this.$labels = this.$optionSelect.find("label");
    this.$optionsContainer = this.$optionSelect.find('.options-container');
    this.$optionList = this.$optionsContainer.children('.js-auto-height-inner');

    if( this.$optionSelect.find('.multiple-choice').length) {
      this.$labels = this.$optionSelect.find(".multiple-choice");

    }
    this.setCheckboxAriaControlsAttributes();
    this.attachCheckedCounter();

    // Performance in ie 6/7 is not good enough to support animating the opening/closing
    // so do not allow option-selects to be collapsible in this case
    var allowCollapsible = (typeof ieVersion == "undefined" || ieVersion > 7) ? true : false;
    if(allowCollapsible) {

      // Attach listener to update checked count
      this.$options.on('click', this.updateCheckedCount.bind(this));

      // Replace div.container-head with a button
      this.replaceHeadWithButton();

      // Add js-collapsible class to parent for CSS
      this.$optionSelect.addClass('js-collapsible');

      // Add open/close listeners
      this.$optionSelect.find('.js-container-head').on('click', this.toggleOptionSelect.bind(this));

      // Add key listeners the option-select is fully usable with a keyboard
      this.$optionSelect.on('focus', this.listenForKeys.bind(this));
      this.$optionSelect.on('blur', this.stopListeningForKeys.bind(this));

      // Add a listener to the checkboxes so if you navigate to them with the keyboard you can definitely see them
      this.$options.on('focus', this.open.bind(this));

      // Store the initial height so it can be referred to later.
      this.$optionsContainer.data('initialHeight', this.$optionsContainer.height());
      this.$optionsContainer.data('maxHeight', this.$optionsContainer.css('maxHeight') || 'none');

      if (this.$optionSelect.data('closed-on-load') == true) {
        this.close();
      }
      else {
        this.setupHeight();
      }
    }
  }

  OptionSelect.prototype.replaceHeadWithButton = function replaceHeadWithButton(){
    /* Replace the div at the head with a button element. This is based on feedback from Léonie Watson.
     * The button has all of the accessibility hooks that are used by screen readers and etc.
     * We do this in the JavaScript because if the JavaScript is not active then the button shouldn't
     * be there as there is no JS to handle the click event.
     */
    var $containerHead = this.$optionSelect.find('.js-container-head');
    var jsContainerHeadHTML = $containerHead.html();

    // Create button and replace the preexisting html with the button.
    var $button = $('<button>');
    $button.addClass('js-container-head');
    //Add type button to override default type submit when this component is used within a form
    $button.attr('type', 'button');
    $button.attr('aria-expanded', true);
    $button.attr('aria-controls', this.$optionSelect.find('.options-container').attr('id'));
    $button.html(jsContainerHeadHTML);
    $containerHead.replaceWith($button);

  };

  OptionSelect.prototype.setCheckboxAriaControlsAttributes = function setCheckboxAriaControlsAttributes(){
    var controls = this.$optionSelect.data('input-aria-controls');
    if (typeof controls === "string" && $('#' + controls).length > 0) {
      this.$optionSelect.find('input[type="checkbox"]').each(function() {
        $(this).attr('aria-controls', controls);
      });
    }
  };

  OptionSelect.prototype.attachCheckedCounter = function attachCheckedCounter(){
    var checkedString = this.checkedString(true);
    this.$optionSelect.find('.js-container-head').append('<div class="js-selected-counter" data-count="' + checkedString.count + '">'+checkedString.string+'</div>');
  };

  OptionSelect.prototype.updateCheckedCount = function updateCheckedCount(){
    var checkedString = this.checkedString(true);
    this.$optionSelect.find('.js-selected-counter').text(checkedString.string).data('count', checkedString.count);
  };

  OptionSelect.prototype.checkedString = function checkedString(onLoad) {
    var count = this.$options.filter(":checked").length;
    var checkedString = "";
    if (count > 0){
      checkedString = this.$options.filter(":checked").parent().find('> label').text().trim();
      // if(onLoad) {
      //   this.close();
      // }
    }

    if (count > 1) {
      checkedString = count + " selected";

    }

    return {
      string: checkedString,
      count: count
    };
  };


  OptionSelect.prototype.toggleOptionSelect = function toggleOptionSelect(e){
    if (this.isClosed()) {
      this.open();
    } else {
      this.close();
    }
    e.preventDefault();
  };

  OptionSelect.prototype.open = function open(){
    if (this.isClosed()) {
      this.$optionSelect.find('.js-container-head').attr('aria-expanded', true);
      this.$optionSelect.removeClass('js-closed');
      if (!this.$optionsContainer.prop('style').height) {
        this.setupHeight();
      }
      $(window).trigger('optionselect.recalculate');
    }
  };

  OptionSelect.prototype.close = function close(){
    this.$optionSelect.addClass('js-closed');
    this.$optionSelect.find('.js-container-head').attr('aria-expanded', false);
  };

  OptionSelect.prototype.isClosed = function isClosed(){
    return this.$optionSelect.hasClass('js-closed');
  };

  OptionSelect.prototype.setContainerHeight = function setContainerHeight(height){
    if (this.$optionsContainer.data('maxHeight') && height !== 'auto') {
      height = Math.min(height, parseInt(this.$optionsContainer.data('maxHeight'), 10));
    }

    this.$optionsContainer.css({
      'max-height': 'none', // Have to clear the 'max-height' set by the CSS in order for 'height' to be applied
      'height': height
    });
  };

  OptionSelect.prototype.isLabelVisible = function isLabelVisible(index, option){
    if (this.$optionList.length < 1) {
      return false;
    }
    var $label = $(option);
    var initialOptionContainerHeight = this.$optionsContainer.height();
    var optionListOffsetTop = this.$optionList.offset().top;
    var distanceFromTopOfContainer = $label.offset().top - optionListOffsetTop;
    return distanceFromTopOfContainer < initialOptionContainerHeight;
  };

  OptionSelect.prototype.getVisibleLabels = function getVisibleLabels(){
    return this.$labels.filter(this.isLabelVisible.bind(this));
  };

  OptionSelect.prototype.setupHeight = function setupHeight(){
    var initialOptionContainerHeight = this.$optionsContainer.data('initialHeight') || this.$optionsContainer.height();
    var height = this.$optionList.height();
    var lastVisibleLabel, position, topBorder, topPadding, lineHeight;
    if (height < initialOptionContainerHeight + 50) {
      // Resize if the list is only slightly bigger than its container
      this.setContainerHeight(height);
      return;
    }

    // Resize to cut last item cleanly in half
    lastVisibleLabel = this.getVisibleLabels().last();

    // Merged the multiple choice markup in here for consistency; this means that the height doesn't always get picked up
    // So I added a check for the parent wrapper.
    // if(lastVisibleLabel.closest('.multiple-choice').length) {
    //   lastVisibleLabel = lastVisibleLabel.closest('.multiple-choice');
    // }

    if(lastVisibleLabel.length) {
      position = lastVisibleLabel.position().top;
    } else {
      position = 0;
    }


    topBorder = parseInt(lastVisibleLabel.css('border-top-width') || 0, 10);
    topPadding = parseInt(lastVisibleLabel.css('padding-top') || 0, 10);
    if ("normal" == lastVisibleLabel.css('line-height')) {
      lineHeight = parseInt(lastVisibleLabel.css('font-size'), 10);
    } else {
      lineHeight = parseInt(lastVisibleLabel.css('line-height'), 10);
    }

    this.setContainerHeight(position + topBorder + topPadding + (lineHeight / 2));

  };

  OptionSelect.prototype.listenForKeys = function listenForKeys(){
    this.$optionSelect.keypress(this.checkForSpecialKeys.bind(this));
  };

  OptionSelect.prototype.checkForSpecialKeys = function checkForSpecialKeys(e){
    if(e.keyCode == 13) {

      // keyCode 13 is the return key.
      this.toggleOptionSelect();
    }
  };

  OptionSelect.prototype.stopListeningForKeys = function stopListeningForKeys(){
    this.$optionSelect.unbind('keypress');
  };

  GOVUK.OptionSelect = OptionSelect;

  // Instantiate an option select for each one found on the page
  var filters = $('.govuk-option-select').map(function(){
    return new GOVUK.OptionSelect({$el:$(this)});
  });

  // Collapsed / hidden elements don't play ball so when we trigger a showhide event,
  // we need to recalculate the height.
  $(window).on('showhide.show', function() {
    filters.each(function() {
      this.setContainerHeight('auto');
      this.setupHeight();
    });
  });

  $(window).on('optionselect.recalculate', function() {
    filters.each(function() {
      this.setContainerHeight('auto');
      this.setupHeight();
    });
  });
})(jQuery);
