import angular from 'angular';

/*
The MIT License (MIT)

Copyright (c) 2014 Austin Andrews (@templarian)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

This is based on the context menu code available here:
 https://github.com/Templarian/ui.bootstrap.contextMenu/blob/6f8d787726019ef9e814fbfbd986f10aaf74eddd/contextMenu.js
*/

/** @ngInject */
export function contextMenuAsDirective() {
  'use strict';
  return ["$parse", function ($parse) {

    const _contextMenus = [] as Array<JQuery<HTMLElement>>;
    // Contains the element that was clicked to show the context menu
    let _clickedElement: any = null;
    const DEFAULT_ITEM_TEXT = "New Item";

    function createAndAddOptionText(params) {
      // Destructuring:
      let $scope = params.$scope;
      let item = params.item;
      let event = params.event;
      let modelValue = params.modelValue;
      let $promises = params.$promises;
      let nestedMenu = params.nestedMenu;
      let $li = params.$li;
      let leftOriented = String(params.orientation).toLowerCase() === 'left';

      let optionText: JQuery<HTMLElement> | null = null;

      if (item.html) {
        if (angular.isFunction(item.html)) {
          // runs the function that expects a jQuery/jqLite element
          optionText = item.html($scope);
        } else {
          // Assumes that the developer already placed a valid jQuery/jqLite element
          optionText = item.html;
        }
      } else {

        let $a = $('<a>');
        let $anchorStyle = {} as any;

        if (leftOriented) {
          $anchorStyle.textAlign = 'right';
          $anchorStyle.paddingLeft = "8px";
        } else {
          $anchorStyle.textAlign = 'left';
          $anchorStyle.paddingRight = "8px";
        }

        $a.css($anchorStyle);
        $a.attr({ tabindex: '-1', href: '#' });

        const textParam = item[0];
        let text = DEFAULT_ITEM_TEXT;

        if (typeof textParam === 'string') {
          text = textParam;
        } else if (typeof textParam === "function") {
          text = textParam.call($scope, $scope, event, modelValue);
        } else if (typeof item.text !== "undefined") {
          text = item.text;
        }

        const $promise = Promise.resolve(text);
        $promises.push($promise);
        $promise.then(function (pText) {
          if (nestedMenu) {
            let $arrow;
            const $boldStyle = {
              fontFamily: 'monospace',
              fontWeight: 'bold',
            } as any;

            if (leftOriented) {
              $arrow = '&lt;';
              $boldStyle.float = 'left';
            } else {
              $arrow = '&gt;';
              $boldStyle.float = 'right';
            }

            const $bold = $('<strong style="font-family:monospace;font-weight:bold;float:right;">' + $arrow + '</strong>');
            $bold.css($boldStyle);
            $a.css("cursor", "default");
            $a.append($bold);
          }
          $a.append(pText);
        });

        optionText = $a;
      }

      $li.append(optionText);
    }

    /**
     * Process each individual item
     *
     * Properties of params:
     * - $scope
     * - event
     * - modelValue
     * - level
     * - item
     * - $ul
     * - $li
     * - $promises
     */
    function processItem(params) {
      const nestedMenu = extractNestedMenu(params);

      // if html property is not defined, fallback to text, otherwise use default text
      // if first item in the item array is a function then invoke .call()
      // if first item is a string, then text should be the string.

      const currItemParam = angular.extend({}, params);
      currItemParam.nestedMenu = nestedMenu;
      currItemParam.enabled = isOptionEnabled(currItemParam);
      currItemParam.text = createAndAddOptionText(currItemParam);

      registerCurrentItemEvents(currItemParam);

    }

    /*
     * Registers the appropriate mouse events for options if the item is enabled.
     * Otherwise, it ensures that clicks to the item do not propagate.
     */
    function registerCurrentItemEvents (params) {
      // Destructuring:
      const item = params.item;
      let $ul = params.$ul;
      let $li = params.$li;
      let $scope = params.$scope;
      let modelValue = params.modelValue;
      let level = params.level;
      let event = params.event;
      let text = params.text;
      let nestedMenu = params.nestedMenu;
      let enabled = params.enabled;
      let orientation = String(params.orientation).toLowerCase();

      if (enabled) {
        const openNestedMenu = function ($event) {
          removeContextMenus(level + 1);
          /*
           * The object here needs to be constructed and filled with data
           * on an "as needed" basis. Copying the data from event directly
           * or cloning the event results in unpredictable behavior.
           */
          /// adding the original event in the object to use the attributes of the mouse over event in the promises
          const ev = {
            pageX: orientation === 'left' ? event.pageX - $ul[0].offsetWidth + 1 : event.pageX + $ul[0].offsetWidth - 1,
            pageY: $ul[0].offsetTop + $li[0].offsetTop - 3,
            view: event.view || window,
            target: event.target,
            event: $event
          };

          /*
           * At this point, nestedMenu can only either be an Array or a promise.
           * Regardless, passing them to `when` makes the implementation singular.
           */
          Promise.resolve(nestedMenu).then(function(promisedNestedMenu) {
            const nestedParam = {
              "$scope" : $scope,
              "event" : ev,
              "options" : promisedNestedMenu,
              "modelValue" : modelValue,
              "level" : level + 1,
              "orientation": orientation
            };
            renderContextMenu(nestedParam);
          });
        };

        $li.on('click', function ($event) {
          if($event.which === 1) {
            $event.preventDefault();
            $scope.$apply(function () {
              $(event.currentTarget).removeClass('context');
              removeAllContextMenus();

              const clickFunction = angular.isFunction(item.click)
                ? item.click
                : (angular.isFunction(item[1])
                  ? item[1]
                  : null);

              if (clickFunction) {
                clickFunction.call($scope, $scope, event, modelValue, text, $li);
              }
            });
          }
        });

        $li.on('mouseover', function ($event) {
          $scope.$apply(function () {
            if (nestedMenu) {
              openNestedMenu($event);
            } else {
              removeContextMenus(level + 1);
            }
          });
        });
      } else {
        $li.on('click', function ($event) {
          $event.preventDefault();
        });
        $li.addClass('disabled');
      }
    }

    /**
     * @param params - an object containing the `item` parameter
     * @returns an Array or a Promise containing the children,
     *          or null if the option has no submenu
     */
    function extractNestedMenu(params) {
      // Destructuring:
      const item = params.item;

      // New implementation:
      if (item.children) {
        if (angular.isFunction(item.children)) {
          // Expects a function that returns a Promise or an Array
          return item.children();
        } else if (angular.isFunction(item.children.then) || angular.isArray(item.children)) {
          // Returns the promise
          // OR, returns the actual array
          return item.children;
        }

        return null;

      } else {
        // nestedMenu is either an Array or a Promise that will return that array.
        // NOTE: This might be changed soon as it's a hangover from an old implementation

        return angular.isArray(item[1]) ||
        (item[1] && angular.isFunction(item[1].then)) ? item[1] : angular.isArray(item[2]) ||
        (item[2] && angular.isFunction(item[2].then)) ? item[2] : angular.isArray(item[3]) ||
        (item[3] && angular.isFunction(item[3].then)) ? item[3] : null;
      }
    }

    /**
     * Responsible for the actual rendering of the context menu.
     *
     * The parameters in params are:
     * - $scope = the scope of this context menu
     * - event = the event that triggered this context menu
     * - options = the options for this context menu
     * - modelValue = the value of the model attached to this context menu
     * - level = the current context menu level (defauts to 0)
     * - customClass = the custom class to be used for the context menu
     */
    function renderContextMenu (params) {
      /// <summary>Render context menu recursively.</summary>

      // Destructuring:
      const options = params.options;

      // Initialize the container. This will be passed around
      const $ul = initContextMenuContainer(params);
      params.$ul = $ul;

      // Register this level of the context menu
      _contextMenus.push($ul);

      /*
       * This object will contain any promises that we have
       * to wait for before trying to adjust the context menu.
       */
      const $promises = [];
      params.$promises = $promises;

      angular.forEach(options, function (item) {
        const $li = $('<li>');
        if (item === null) {
          $li.addClass('divider');
        } else if (typeof item[0] === "object") {
          //custom.initialize($li, item);
        } else {
          const itemParams = angular.extend({}, params);
          itemParams.item = item;
          itemParams.$li = $li;
          processItem(itemParams);
        }
        $ul.append($li);
      });

      $(document).find('body').append($ul);

      doAfterAllPromises(params);
    }

    /**
     * calculate if drop down menu would go out of screen at left or bottom
     * calculation need to be done after element has been added (and all texts are set; thus the promises)
     * to the DOM the get the actual height
     */
    function doAfterAllPromises (params) {

      // Desctructuring:
      const $ul = params.$ul;
      const $promises = params.$promises;
      const level = params.level;
      const event = params.event;
      const leftOriented = String(params.orientation).toLowerCase() === 'left';

      Promise.all($promises).then(function () {
        let topCoordinate  = event.pageY;
        const menuHeight = angular.element($ul[0]).prop('offsetHeight');
        const winHeight = window.scrollY + event.view.innerHeight;
        /// the 20 pixels in second condition are considering the browser status bar that sometimes overrides the element
        if (topCoordinate > menuHeight && winHeight - topCoordinate < menuHeight + 20) {
          topCoordinate = event.pageY - menuHeight;
          /// If the element is a nested menu, adds the height of the parent li to the topCoordinate to align with the parent
          if(level && level > 0) {
            topCoordinate += event.event.currentTarget.offsetHeight;
          }
        } else if(winHeight <= menuHeight) {
          // If it really can't fit, reset the height of the menu to one that will fit
          angular.element($ul[0]).css({"height": winHeight - 5, "overflow-y": "scroll"});
          // ...then set the topCoordinate height to 0 so the menu starts from the top
          topCoordinate = 0;
        } else if(winHeight - topCoordinate < menuHeight) {
          let reduceThresholdY = 5;
          if(topCoordinate < reduceThresholdY) {
            reduceThresholdY = topCoordinate;
          }
          topCoordinate = winHeight - menuHeight - reduceThresholdY;
        }

        let leftCoordinate = event.pageX;
        const menuWidth = angular.element($ul[0]).prop('offsetWidth');
        const winWidth = event.view.innerWidth;
        const padding = 5;

        if (leftOriented) {
          if (winWidth - leftCoordinate > menuWidth && leftCoordinate < menuWidth + padding) {
            leftCoordinate = padding;
          } else if (leftCoordinate < menuWidth) {
            let reduceThresholdX = 5;
            if (winWidth - leftCoordinate < reduceThresholdX + padding) {
              reduceThresholdX = winWidth - leftCoordinate + padding;
            }
            leftCoordinate = menuWidth + reduceThresholdX + padding;
          } else {
            leftCoordinate = leftCoordinate - menuWidth;
          }
        } else {
          if (leftCoordinate > menuWidth && winWidth - leftCoordinate - padding < menuWidth) {
            leftCoordinate = winWidth - menuWidth - padding;
          } else if(winWidth - leftCoordinate < menuWidth) {
            let reduceThresholdX = 5;
            if(leftCoordinate < reduceThresholdX + padding) {
              reduceThresholdX = leftCoordinate + padding;
            }
            leftCoordinate = winWidth - menuWidth - reduceThresholdX - padding;
          }
        }

        $ul.css({
          display: 'block',
          position: 'absolute',
          left: leftCoordinate + 'px',
          top: topCoordinate + 'px'
        });
      });

    }

    /**
     * Creates the container of the context menu (a <ul> element),
     * applies the appropriate styles and then returns that container
     *
     * @return a <ul> jqLite/jQuery element
     */
    function initContextMenuContainer(params) {
      // Destructuring
      const customClass = params.customClass;

      const $ul = $('<ul>');
      $ul.addClass('dropdown-menu');
      $ul.attr({ 'role': 'menu' });
      $ul.css({
        display: 'block',
        position: 'absolute',
        left: params.event.pageX + 'px',
        top: params.event.pageY + 'px',
        "z-index": 10000
      });

      if(customClass) { $ul.addClass(customClass); }

      return $ul;
    }

    // if item is object, and has enabled prop invoke the prop
    // else if fallback to item[2]
    function isOptionEnabled (params) {
      const item = params.item;
      const $scope = params.$scope;
      const event = params.event;
      const modelValue = params.modelValue;
      const text = params.text;

      if (typeof item.enabled !== "undefined") {
        return item.enabled.call($scope, $scope, event, modelValue, text);
      } else if (typeof item[2] === "function") {
        return item[2].call($scope, $scope, event, modelValue, text);
      } else {
        return true;
      }
    }

    function isTouchDevice() {
      return 'ontouchstart' in window  || navigator.maxTouchPoints; // works on most browsers | works on IE10/11 and Surface
    }

    /**
     * Removes the context menus with level greater than or equal
     * to the value passed. If undefined, null or 0, all context menus
     * are removed.
     */
    function removeContextMenus (level?: number) {
      while (_contextMenus.length > 0 && (!level || _contextMenus.length > level)) {
        const entry = _contextMenus.pop();
        if (entry) {
          entry.remove();
        }
      }
    }

    function removeOnScrollEvent(e) {
      removeAllContextMenus(e);
    }

    function removeOnOutsideClickEvent(e) {
      let $curr = $(e.target);
      let shouldRemove = true;

      while($curr.length) {
        if($curr.hasClass("dropdown-menu")) {
          shouldRemove = false;
          break;
        } else {
          $curr = $curr.parent();
        }
      }
      if (shouldRemove) {
        removeAllContextMenus(e);
      }
    }

    function removeAllContextMenus(_event?: Event) {
      $(document.body).off('mousedown', removeOnOutsideClickEvent);
      $(document).off('scroll', removeOnScrollEvent);
      $(_clickedElement).removeClass('context');
      removeContextMenus();
    }

    return function ($scope: any, el: Element, attrs) {
      const element = $(el);

      let openMenuEvent = "contextmenu";
      if(attrs.contextMenuOn && typeof(attrs.contextMenuOn) === "string"){
        openMenuEvent = attrs.contextMenuOn;
      }
      element.on(openMenuEvent, (event: Event) => {
        if(!attrs.allowEventPropagation) {
          event.stopPropagation();
          event.preventDefault();
        }

        // Don't show context menu if on touch device and element is draggable
        if(isTouchDevice() && element.attr('draggable') === 'true') {
          return false;
        }

        // Remove if the user clicks outside
        $(document.body).on('mousedown', removeOnOutsideClickEvent);
        // Remove the menu when the scroll moves
        $(document).on('scroll', removeOnScrollEvent);

        _clickedElement = event.currentTarget;
        ($(_clickedElement) as JQLite).addClass('context');

        $scope.$apply(function () {
          $scope.$event = event;
          const options = $scope.$eval(attrs.contextMenu);
          const customClass = attrs.contextMenuClass;
          const modelValue = $scope.$eval(attrs.model);
          const orientation = attrs.contextMenuOrientation;

          const params = {
            "$scope" : $scope,
            "event" : event,
            "options" : options,
            "modelValue" : modelValue,
            "level" : 0,
            "customClass" : customClass,
            "orientation": orientation
          };

          if (options instanceof Array) {
            if (options.length === 0) { return; }
            renderContextMenu(params);
          } else {
            throw '"' + attrs.contextMenu + '" not an array';
          }
        });

        // Remove all context menus if the scope is destroyed
        $scope.$on("$destroy", function () {
          removeAllContextMenus();
        });

        return true;
      });
    };
  }];
}

