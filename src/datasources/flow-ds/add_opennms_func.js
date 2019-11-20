import _ from 'lodash';

import {Gfuncs, Cardinality} from "./flow_functions";

angular
  .module('grafana.directives')
  .directive('opennmsAddFunc', function ($compile) {
    let inputTemplate = '<input type="text"' +
      ' class="gf-form-input"' +
      ' spellcheck="false" style="display:none"></input>';

    let buttonTemplate = '<a  class="gf-form-label query-part dropdown-toggle"' +
      ' tabindex="1" gf-dropdown="functionMenu" data-toggle="dropdown">' +
      '<i class="fa fa-plus"></i></a>';

    return {
      scope: {
        segmentMetric: '@',
        ctrl: '<'
      },
      link: function ($scope, elem) {
        let ctrl = $scope.ctrl;
        let categories = Gfuncs.getCategories();
        let allFunctions = getAllFunctionNames(categories);

        let renderFunction = (segmentValue, selectedFunctions) => {
          // Clear the previous content of the button
          elem.empty();
          $scope.functionMenu = createFunctionDropDownMenu(categories, segmentValue, selectedFunctions);

          let $input = $(inputTemplate);
          let $button = $(buttonTemplate);
          $input.appendTo(elem);
          $button.appendTo(elem);

          $input.attr('data-provide', 'typeahead');
          $input.typeahead({
            source: allFunctions,
            minLength: 1,
            items: 10,
            updater: function (value) {
              let funcDef = Gfuncs.getFuncDef(value);
              if (!funcDef) {
                // try find close match
                value = value.toLowerCase();
                funcDef = _.find(allFunctions, function (funcName) {
                  return funcName.toLowerCase().indexOf(value) === 0;
                });

                if (!funcDef) {
                  return;
                }
              }

              $scope.$apply(function () {
                ctrl.addFunction(funcDef);
              });

              $input.trigger('blur');
              return '';
            }
          });

          $button.click(function () {
            $button.hide();
            $input.show();
            $input.focus();
          });

          $input.keyup(function () {
            elem.toggleClass('open', $input.val() === '');
          });

          $input.blur(function () {
            // clicking the function dropdown menu wont
            // work if you remove class at once
            setTimeout(function () {
              $input.val('');
              $input.hide();
              $button.show();
              elem.removeClass('open');
            }, 200);
          });

          $compile(elem.contents())($scope);
        };

        // Set up a watch to re-render the function menu button every time a function is added to hide new functions
        // based on their Cardinality and whether or not they have already been selected
        $scope.$watch('ctrl.target.functions', function(newFunctions) {
          renderFunction($scope.segmentMetric, newFunctions);
        });

        // Set up a watch to re-render the function menu button every time the segment metric changes since some
        // functions are context-aware depending on which metric is selected
        $scope.$watch('segmentMetric', function (newSegmentValue) {
          renderFunction(newSegmentValue, $scope.ctrl.target.functions);
        });
      }
    };
  });

function getAllFunctionNames(categories) {
  return _.reduce(categories, function (list, category) {
    _.each(category, function (func) {
      list.push(func.name);
    });
    return list;
  }, []);
}

function shouldRenderFunctionBasedOnCardinality(f, selectedFunctions) {
  if (!selectedFunctions || !f.cardinality) {
    return true;
  }

  return !(f.cardinality === Cardinality.SINGLE && functionsContainNamedFunction(f.name, selectedFunctions));
}

function isExcluded(f, selectedFunctions) {
    if (!selectedFunctions || !f.mutuallyExcludes) {
        return false;
    }

    return functionsExcludeNamedFunction(f.name, selectedFunctions);
}

function functionsContainNamedFunction(functionName, selectedFunctions) {
  let result = false;
  selectedFunctions.forEach((f) => {
    if (f.name === functionName) {
      result = true;
    }
  });
  return result;
}

function functionsExcludeNamedFunction(functionName, selectedFunctions) {
    let result = false;
    selectedFunctions.forEach((f) => {
        let funcDef = Gfuncs.getFuncDef(f.name);
        if (funcDef.mutuallyExcludes && funcDef.mutuallyExcludes.includes(functionName)) {
            result = true;
        }
    });
    return result;
}

function createFunctionDropDownMenu(categories, selectedSegment, selectedFunctions) {
  let categoriesToRender = [];

  _.forEach(categories, (functionsInCategory, categoryName) => {
    let functionsToRender = [];
    functionsInCategory.forEach((item) => {
      // Only add this submenu item if it is applicable to the currently selected metric segment and its Cardinality
      // isn't preventing it from being added
      if ((!item.appliesToSegments || item.appliesToSegments.includes(selectedSegment)) &&
          shouldRenderFunctionBasedOnCardinality(item, selectedFunctions) && !isExcluded(item, selectedFunctions)) {
        functionsToRender.push(item);
      }
    });

    let submenu = _.map(functionsToRender, (f) => {
      return {
        text: f.name,
        click: "ctrl.addFunction('" + f.name + "')",
      };
    });

    // Only include this category if it has at least one function to display
    if (submenu.length > 0) {
      categoriesToRender.push({
        text: categoryName,
        submenu: submenu
      });
    }
  });

  return categoriesToRender;
}
