
/** javascripts/application.js **/
/* global $ */
/* global jQuery */
/* global GOVUK */


$(document).ready(function () {
    // Turn off jQuery animation
    jQuery.fx.off = true;

    // Where .multiple-choice uses the data-target attribute
    // to toggle hidden content
    var showHideContentFoo = new GOVUK.ShowHideContentFoo();

    showHideContentFoo.init();

    $('input.volume').keyup(function(event) {

        // format number
        $(this).val(function(index, value) {
            return value
                .replace(/\D/g, "")
                .replace(/\B(?=(\d{3})+(?!\d))/g, ",");
        });
    });

    var errorSummary = $('#error-summary-display');
    //focus error summary on page load
    if(errorSummary.length) {
        $(document).scrollTop(errorSummary.offset().top);
        $(errorSummary).focus();
    }
});

window.onload = function () {
    if (document.getElementsByClassName("flash error-summary error-summary--show").length > 0) {
        ga('send', 'event', 'validationError', 'error', window.location.pathname);
    }

    if (window.location.href.indexOf("start") > -1) {
        if (document.getElementById("registration-status").textContent.indexOf("agent") > -1) {
            ga('send', 'event', 'agents', 'visited', 'agentsPageVisited');
        } else if (document.getElementById("registration-status").textContent.indexOf("assistant") > -1) {
            ga('send', 'event', 'assistants', 'visited', 'assistantsPageVisited');
        } else if (document.getElementById("registration-status").textContent.indexOf("already registered") > -1) {
            ga('send', 'event', 'alreadyRegistered', 'visited', 'alreadyRegisteredPageVisited');
        }

    } else if (window.location.href.indexOf("verify") > -1 && document.getElementById("registration-pending-title")) {
        ga('send', 'event', 'pending', 'visited', 'pendingPageVisited');
    }

    // for returns
    if(document.getElementById('exemptions-for-small-producers-true')) {
        document.getElementById('exemptions-for-small-producers-true').addEventListener('click', function() {
            ga('send', 'event', 'smallProducerExemption', 'click', 'Yes');
        });
    }

    $('form[action="exemptions-for-small-producers"] button').wrap("<span id='exemptions-click-wrapper'></span>");
    $('form[action="exemptions-for-small-producers"] #exemptions-click-wrapper').attr('onclick',"ga('send', 'event', 'smallProducerExemptionButton', 'click', 'Submit');");

    $('form[action="check-your-answers"] button').wrap("<span id='cya-click-wrapper'></span>");
    $('form[action="check-your-answers"] #cya-click-wrapper').attr('onclick',"ga('send', 'event', 'checkYourAnswers', 'click', 'Submit');");

    if (document.getElementsByTagName('h1').length > 0 && document.getElementsByTagName('h1')[0].innerText == 'Return submitted') {
        ga('send', 'event', 'visited', 'load', 'Return submitted');
    }

    if (document.getElementsByClassName("error-summary-list").length > 0) {
        ga('send', 'event', 'validationError', 'error', document.getElementsByClassName("error-summary-list").innerText);
    }


    $(".returns-change-link").click(function(event) {
        event.preventDefault();
        var redirectUrl = $(this).attr('href');
        gaWithCallback('send', 'event', 'changeLinks', 'click', redirectUrl, function() {
            window.location.href = redirectUrl;
        });
    });



    function gaWithCallback(send, event, category, action, label, callback) {
        ga(send, event, category, action, label, {
            hitCallback: gaCallback
        });
        var gaCallbackCalled = false;
        setTimeout(gaCallback, 5000);

        function gaCallback() {
            if(!gaCallbackCalled) {
                callback();
                gaCallbackCalled = true;
            }
        }
    }


    $('form[action="organisation-type"] button').wrap("<span id='org-type-click-wrapper'></span>");
    var radioValue = $("input[name='organisation-type']:checked").val();
    $('form[action="organisation-type"] #org-type-click-wrapper').attr('onclick',"ga('send', 'event', 'orgType', 'selectOrg', '"+radioValue+"');");
    //
    // $("#org-type-click-wrapper").click(function(){
    //
    //
    //
    //     if(radioValue){
    //
    //         confirm("Your are a - " + radioValue);
    //
    //     }
    //
    // });

};
/** javascripts/timeout-dialog.js **/

// if(window.location.pathname !== "/soft-drinks-industry-levy/time-out") {
// $.timeoutDialog({
//     timeout: 900,
//     countdown: 120,
//     keep_alive_url: window.location.href,
//     restart_on_yes: true,
//     logout_url: '/soft-drinks-industry-levy/time-out',
//     keep_alive_button_text: 'Stay signed in',
//     sign_out_button_text: 'Sign out'
// })};
/** javascripts/show-hide-content.js **/
;(function (global) {
    'use strict';

    var $ = global.jQuery;
    var GOVUK = global.GOVUK || {};

    function ShowHideContentFoo () {
        var self = this;

        // Radio and Checkbox selectors
        var selectors = {
            namespace: 'ShowHideContent',
            radio: '[data-target] > input[type="radio"]',
            checkbox: '[data-target] > input[type="checkbox"]'
        };

        // Escape name attribute for use in DOM selector
        function escapeElementName (str) {
            return str.replace('[', '\\[').replace(']', '\\]').replace('.', '\\.')
        }

        // Adds ARIA attributes to control + associated content
        function initToggledContent () {
            var $control = $(this);
            var $content = getToggledContent($control);

            // Set aria-controls and defaults
            if ($content.length) {
                $control.attr('aria-controls', $content.attr('id'));
                $control.attr('aria-expanded', 'false');
                $content.attr('aria-hidden', 'true')
            }
        }

        // Return toggled content for control
        function getToggledContent ($control) {
            var id = $control.attr('aria-controls');

            // ARIA attributes aren't set before init
            if (!id) {
                id = $control.closest('[data-target]').data('target')
            }

            // Find show/hide content by id
            return $('#' + id)
        }

        // Show toggled content for control
        function showToggledContent ($control, $content) {
            // Show content
            if ($content.hasClass('js-hidden')) {
                $content.removeClass('js-hidden');
                $content.attr('aria-hidden', 'false');

                // If the controlling input, update aria-expanded
                if ($control.attr('aria-controls')) {
                    $control.attr('aria-expanded', 'true')
                }
            }
        }

        // Hide toggled content for control
        function hideToggledContent ($control, $content) {
            $content = $content || getToggledContent($control);

            // Hide content
            if (!$content.hasClass('js-hidden')) {
                $content.addClass('js-hidden');
                $content.attr('aria-hidden', 'true');

                // If the controlling input, update aria-expanded
                if ($control.attr('aria-controls')) {
                    $control.attr('aria-expanded', 'false')
                }
            }
        }

        // Handle radio show/hide
        function handleRadioContent ($control, $content) {
            // All radios in this group which control content
            var selector = selectors.radio + '[name=' + escapeElementName($control.attr('name')) + '][aria-controls]';
            var $form = $control.closest('form');
            var $radios = $form.length ? $form.find(selector) : $(selector);

            // Hide content for radios in group
            $radios.each(function () {
                hideToggledContent($(this))
            });

            // Select content for this control
            if ($control.is('[aria-controls]')) {
                showToggledContent($control, $content)
            }
        }

        // Handle checkbox show/hide
        function handleCheckboxContent ($control, $content) {
            // Show checkbox content
            if ($control.is(':checked')) {
                showToggledContent($control, $content)
            } else { // Hide checkbox content
                hideToggledContent($control, $content)
            }
        }

        // Set up event handlers etc
        function init ($container, elementSelector, eventSelectors, handler) {
            $container = $container || $(document.body);

            // Handle control clicks
            function deferred () {
                var $control = $(this);
                handler($control, getToggledContent($control))
            }

            // Prepare ARIA attributes
            var $controls = $(elementSelector);
            $controls.each(initToggledContent);

            // Handle events
            $.each(eventSelectors, function (idx, eventSelector) {
                $container.on('click.' + selectors.namespace, eventSelector, deferred)
            });

            // Any already :checked on init?
            if ($controls.is(':checked')) {
                $controls.filter(':checked').each(deferred)
            }
        }

        // Get event selectors for all radio groups
        function getEventSelectorsForRadioGroups () {
            var radioGroups = [];

            // Build an array of radio group selectors
            return $(selectors.radio).map(function () {
                var groupName = $(this).attr('name');

                if ($.inArray(groupName, radioGroups) === -1) {
                    radioGroups.push(groupName);
                    return 'input[type="radio"][name="' + $(this).attr('name') + '"]'
                }
                return null
            })
        }

        // Set up radio show/hide content for container
        self.showHideRadioToggledContent = function ($container) {
            init($container, selectors.radio, getEventSelectorsForRadioGroups(), handleRadioContent)
        };

        // Set up checkbox show/hide content for container
        self.showHideCheckboxToggledContent = function ($container) {
            init($container, selectors.checkbox, [selectors.checkbox], handleCheckboxContent)
        };

        // Remove event handlers
        self.destroy = function ($container) {
            $container = $container || $(document.body);
            $container.off('.' + selectors.namespace)
        }
    }

    ShowHideContentFoo.prototype.init = function ($container) {
        this.showHideRadioToggledContent($container);
        this.showHideCheckboxToggledContent($container)
    };

    GOVUK.ShowHideContentFoo = ShowHideContentFoo;

    global.GOVUK = GOVUK
})(window);

/** javascripts/details.polyfill.js **/
/*! http://mths.be/details v0.1.0 by @mathias | includes http://mths.be/noselect v1.0.3 */
;(function (document, $)
{

    var proto = $.fn,
        details,
        nextDetailsId = 1,
        // :'(
        isOpera = Object.prototype.toString.call(window.opera) == '[object Opera]',
        // Feature test for native `<details>` support
        isDetailsSupported = (function (doc)
        {
            var el = doc.createElement('details'),
                fake,
                root,
                diff;
            if (!('open' in el))
            {
                return false;
            }
            root = doc.body || (function ()
            {
                var de = doc.documentElement;
                fake = true;
                return de.insertBefore(doc.createElement('body'), de.firstElementChild || de.firstChild);
            }());
            el.innerHTML = '<summary>a</summary><div>b</div>';
            el.style.display = 'block';
            root.appendChild(el);
            diff = el.offsetHeight;
            el.open = true;
            diff = diff != el.offsetHeight;
            root.removeChild(el);
            if (fake)
            {
                root.parentNode.removeChild(root);
            }
            return diff;
        }(document)),
        fixDetailContentId = function ($detailsNotSummary)
        {
            var $content = $detailsNotSummary.first();
            if (!$content.attr("id"))
            {
                $content.attr("id", "details-content-" + nextDetailsId++);
            }
        },
        toggleOpen = function ($details, $detailsSummary, $detailsNotSummary, toggle)
        {
            var isOpen = $details.prop('open'),
                close = isOpen && toggle || !isOpen && !toggle;
            if (close)
            {
                $details.removeClass('open').prop('open', false).removeAttr("open").triggerHandler('close.details');
                $detailsSummary.attr('aria-expanded', false);
                $detailsNotSummary.hide();
            }
            else
            {
                $details.addClass('open').prop('open', true).attr("open", "").triggerHandler('open.details');
                $detailsSummary.attr('aria-expanded', true);
                $detailsNotSummary.show();
            }
        };

    /* http://mths.be/noselect v1.0.3 */
    proto.noSelect = function ()
    {

        // Since the string 'none' is used three times, storing it in a variable gives better results after minification
        var none = 'none';

        // onselectstart and ondragstart for WebKit & IE
        // onmousedown for WebKit & Opera
        return this.bind('selectstart dragstart mousedown', function ()
        {
            return false;
        }).css({
            'MozUserSelect': none,
            'msUserSelect': none,
            'webkitUserSelect': none,
            'userSelect': none
        });

    };

    // Execute the fallback only if there’s no native `details` support
    if (isDetailsSupported)
    {

        details = proto.details = function ()
        {
            return this.each(function ()
            {
                var $details = $(this),
                    $summary = $('summary', $details).first();

                if ($details.prop("details-initialised"))
                    return;

                fixDetailContentId($details.children(':not(summary)'));

                $details.prop("details-initialised", true);
                $summary.attr({
                    'role': 'button',
                    'aria-expanded': $details.prop('open')
                }).on('click', function ()
                {
                    // the value of the `open` property is the old value
                    var isOpen = $details.prop('open');
                    $summary.attr('aria-expanded', !isOpen);
                    $details.toggleClass("open", !isOpen).triggerHandler((isOpen ? 'close' : 'open') + '.details');
                }).on("toggle-open", function ()
                {
                    var opened = $details.prop('open');
                    $details.prop("open", !opened);
                    if (opened)
                    {
                        $details.removeClass("open").removeAttr("open");
                    }
                    else
                    {
                        $details.addClass("open").attr("open", "");
                    }
                    $summary.attr('aria-expanded', !opened);
                    $details.triggerHandler((opened ? 'close' : 'open') + '.details');
                });
            });

        };

        details.support = isDetailsSupported;

    }
    else
    {

        details = proto.details = function ()
        {

            // Loop through all `details` elements
            return this.each(function ()
            {

                // Store a reference to the current `details` element in a variable
                var $details = $(this),
                    // Store a reference to the `summary` element of the current `details` element (if any) in a variable
                    $detailsSummary = $('summary', $details).first(),
                    // Do the same for the info within the `details` element
                    $detailsNotSummary = $details.children(':not(summary)'),
                    // This will be used later to look for direct child text nodes
                    $detailsNotSummaryContents = $details.contents(':not(summary)');

                if ($details.prop("details-initialised"))
                {
                    return;
                }

                $details.attr("role", "group");
                $details.prop("details-initialised", true);

                // If there is no `summary` in the current `details` element…
                if (!$detailsSummary.length)
                {
                    // …create one with default text
                    $detailsSummary = $('<summary>').text('Details').prependTo($details);
                }

                $('<i>').addClass("arrow arrow-open").append(document.createTextNode("\u25bc")).prependTo($detailsSummary);
                $('<i>').addClass("arrow arrow-closed").append(document.createTextNode("\u25ba")).prependTo($detailsSummary);

                // Look for direct child text nodes
                if ($detailsNotSummary.length != $detailsNotSummaryContents.length)
                {
                    // Wrap child text nodes in a `span` element
                    $detailsNotSummaryContents.filter(function ()
                    {
                        // Only keep the node in the collection if it’s a text node containing more than only whitespace
                        // http://www.whatwg.org/specs/web-apps/current-work/multipage/common-microsyntaxes.html#space-character
                        return this.nodeType == 3 && /[^ \t\n\f\r]/.test(this.data);
                    }).wrap('<span>');
                    // There are now no direct child text nodes anymore — they’re wrapped in `span` elements
                    $detailsNotSummary = $details.children(':not(summary)');
                }

                fixDetailContentId($detailsNotSummary);

                // Hide content unless there’s an `open` attribute
                $details.prop('open', typeof $details.attr('open') == 'string');
                toggleOpen($details, $detailsSummary, $detailsNotSummary);

                // Add `role=button` and set the `tabindex` of the `summary` element to `0` to make it keyboard accessible
                $detailsSummary.attr('role', 'button').noSelect().prop('tabIndex', 0)
                    .on('click', function ()
                    {
                        // Focus on the `summary` element
                        $detailsSummary.focus();
                        // Toggle the `open` and `aria-expanded` attributes and the `open` property of the `details` element and display the additional info
                        toggleOpen($details, $detailsSummary, $detailsNotSummary, true);
                    })
                    .on("toggle-open", function ()
                    {
                        // Toggle the `open` and `aria-expanded` attributes and the `open` property of the `details` element and display the additional info
                        toggleOpen($details, $detailsSummary, $detailsNotSummary, true);
                    })
                    .keyup(function (event)
                    {
                        if (32 == event.keyCode || (13 == event.keyCode && !isOpera))
                        {
                            // Space or Enter is pressed — trigger the `click` event on the `summary` element
                            // Opera already seems to trigger the `click` event when Enter is pressed
                            event.preventDefault();
                            $detailsSummary.click();
                        }
                    });

            });

        };

        details.support = isDetailsSupported;
    }

    if (!isDetailsSupported)
    {
        $("html").addClass("no-details");
    }

    $(window).on("reapplyDetails", function ()
    {
        $("details").details();
    });

    $(function ()
    {
        $("details").details();
    });
}(document, jQuery));
