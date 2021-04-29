import _ from 'lodash';
import angular from 'angular';

import { FuncInstance } from './flow_functions';

const move = (array: any[], fromIndex: number, toIndex: number) => {
  array.splice(toIndex, 0, array.splice(fromIndex, 1)[0]);
};

/**
 * This function editor has been adapted from the Graphite query editor's functione editor.
 */
angular
  .module('grafana.directives')
  .directive('opennmsFuncEditor', function ($compile: angular.ICompileService, templateSrv: any, timeSrv: any) {

    const funcSpanTemplate = '<a ng-click="">{{func.def.name}}</a><span>(</span>';
    const paramTemplate = '<input type="text" style="display:none"' +
      ' class="input-small tight-form-func-param"></input>';

    const funcControlsTemplate =
      '<div class="flows-editor tight-form-func-controls" style="display: none">' +
      '<span class="pointer fa fa-arrow-left"></span>' +
      '<span class="pointer fa fa-remove" ></span>' +
      '<span class="pointer fa fa-arrow-right"></span>' +
      '</div>';

    return {
      restrict: 'A',
      link: function postLink($scope: any, elem: any) {
        // @ts-ignore
        const self = this;

        const $funcLink = $(funcSpanTemplate);
        const $funcControls = $(funcControlsTemplate);
        const ctrl = $scope.ctrl;
        const func = $scope.func;
        let scheduledRelink = false;
        let paramCountAtLink = 0;
        let cancelBlur = null as null | number;

        function clickFuncParam(paramIndex) {
          const $link = $(self);
          const $comma = $link.prev('.comma');
          const $input = $link.next();

          $input.val(func.params[paramIndex]);

          $comma.removeClass('query-part__last');
          $link.hide();
          $input.show();
          $input.focus();
          $input.select();

          let typeahead = $input.data('typeahead');
          if (typeahead) {
            $input.val('');
            typeahead.lookup();
          }
        }

        function scheduledRelinkIfNeeded() {
          if (paramCountAtLink === func.params.length) {
            return;
          }

          if (!scheduledRelink) {
            scheduledRelink = true;
            setTimeout(function () {
              relink();
              scheduledRelink = false;
            }, 200);
          }
        }

        function paramDef(index) {
          if (index < func.def.params.length) {
            return func.def.params[index];
          }
          if ((_.last(func.def.params) as any).multiple) {
            return _.assign({}, _.last(func.def.params), {optional: true});
          }
          return {};
        }

        function switchToLink(inputElem, paramIndex) {
          var $input = $(inputElem);

          if (cancelBlur) {
            // @ts-ignore
            clearTimeout(cancelBlur);
          }
          cancelBlur = null;

          var $link = $input.prev();
          var $comma = $link.prev('.comma');
          var newValue = $input.val();

          // remove optional empty params
          if (newValue !== '' || paramDef(paramIndex).optional) {
            func.updateParam(newValue, paramIndex);
            $link.html(newValue ? templateSrv.highlightVariablesAsHtml(newValue) : '&nbsp;');
          }

          scheduledRelinkIfNeeded();

          $scope.$apply(function() {
            ctrl.targetChanged();
          });

          if ($link.hasClass('query-part__last') && newValue === '') {
            $comma.addClass('query-part__last');
          } else {
            $link.removeClass('query-part__last');
          }

          $input.hide();
          $link.show();
        }

        // this = input element
        function inputBlur(paramIndex) {
          // @ts-ignore
          var inputElem = this;
          // happens long before the click event on the typeahead options
          // need to have long delay because the blur
          // @ts-ignore
          cancelBlur = setTimeout(function() {
            switchToLink(inputElem, paramIndex);
          }, 200);
        }

        function inputKeyPress(paramIndex, e) {
          if (e.which === 13) {
            // @ts-ignore
            $(this).blur();
          }
        }

        function inputKeyDown() {
          const me = self as any;
          me.style.width = (3 + me.value.length) * 8 + 'px';
        }

        function addTypeahead($input: any, paramIndex: number, optionsUpdater?: any) {
          $input.attr('data-provide', 'typeahead');

          let options;
          if (optionsUpdater) {
            options = [] as any[];
            $input.data('optionsUpdater', optionsUpdater);
          } else {
            options = paramDef(paramIndex).options;
          }
          if (paramDef(paramIndex).type === 'int') {
            options = _.map(options, function (val) {
              return val.toString();
            });
          }

          $input.typeahead({
            source: options,
            minLength: 0,
            items: 20,
            updater: function (value) {
              $input.val(value);
              switchToLink($input[0], paramIndex);
              return value;
            }
          });

          let typeahead = $input.data('typeahead');
          typeahead.lookup = function () {
            const me = self as any;
            me.query = me.$element.val() || '';

            if (me.$element.data('optionsUpdater')) {
              if (me.$element.val().length > 0) {
                me.$element.data('optionsUpdater')(
                  me.$element.val(),
                  new OptionsContext(timeSrv, $scope.ctrl.functions, $scope.ctrl.datasource.client)
                ).then((data) => {
                  typeahead.source = data;
                  return me.process(me.source);
                });
              } else {
                typeahead.source = [] as any[];
              }
            }
            return me.process(me.source);
          };
        }

        function toggleFuncControls() {
          let targetDiv = elem.closest('.tight-form');

          if (elem.hasClass('show-function-controls')) {
            elem.removeClass('show-function-controls');
            targetDiv.removeClass('has-open-function');
            $funcControls.hide();
            return;
          }

          elem.addClass('show-function-controls');
          targetDiv.addClass('has-open-function');

          $funcControls.show();
        }

        function addElementsAndCompile() {
          $funcControls.appendTo(elem);
          $funcLink.appendTo(elem);

          var defParams = _.clone(func.def.params) as any[];
          var lastParam = _.last(func.def.params) as any;

          while (func.params.length >= defParams.length && lastParam && lastParam.multiple) {
            defParams.push(_.assign({}, lastParam, {optional: true}));
          }

          _.each(defParams, function(param, index: number) {
            if (param.optional && func.params.length < index) {
              return;
            }

            var paramValue = templateSrv.highlightVariablesAsHtml(func.params[index]);

            var last = (index >= func.params.length - 1) && param.optional && !paramValue;
            if (last && param.multiple) {
              paramValue = '+';
            }

            if (index > 0) {
              $('<span class="comma' + (last ? ' query-part__last' : '') + '">, </span>').appendTo(elem);
            }

            var $paramLink = $(
              '<a ng-click="" class="flows-func-param-link' + (last ? ' query-part__last' : '') + '">'
              + (paramValue || '&nbsp;') + '</a>');
            var $input = $(paramTemplate);
            $input.attr('placeholder', param.name);

            paramCountAtLink++;

            $paramLink.appendTo(elem);
            $input.appendTo(elem);

            $input.blur(_.partial(inputBlur, index));
            $input.keyup(inputKeyDown);
            $input.keypress(_.partial(inputKeyPress, index));
            $paramLink.click(_.partial(clickFuncParam, index));

            if (param.options) {
              if (typeof paramDef(index).options === 'function') {
                addTypeahead($input, index, paramDef(index).options);
              }
              addTypeahead($input, index);
            }

          });

          $('<span>)</span>').appendTo(elem);

          $compile(elem.contents())($scope);
        }

        function ifJustAddedFocusFirstParam() {
          if ($scope.func.added) {
            $scope.func.added = false;
            setTimeout(function () {
              elem.find('.flows-func-param-link').first().click();
            }, 10);
          }
        }

        function registerFuncControlsToggle() {
          $funcLink.click(toggleFuncControls);
        }

        function registerFuncControlsActions() {
          $funcControls.click(function (e) {
            let $target = $(e.target);
            if ($target.hasClass('fa-remove')) {
              toggleFuncControls();
              $scope.$apply(function () {
                ctrl.removeFunction($scope.func);
              });
              return;
            }

            if ($target.hasClass('fa-arrow-left')) {
              $scope.$apply(function () {
                move(ctrl.functions, $scope.$index, $scope.$index - 1);
                ctrl.targetChanged();
              });
              return;
            }

            if ($target.hasClass('fa-arrow-right')) {
              $scope.$apply(function () {
                move(ctrl.functions, $scope.$index, $scope.$index + 1);
                ctrl.targetChanged();
              });
              return;
            }
          });
        }

        function relink() {
          elem.children().remove();

          addElementsAndCompile();
          ifJustAddedFocusFirstParam();
          registerFuncControlsToggle();
          registerFuncControlsActions();
        }

        relink();
      }
    };

  });

class OptionsContext {
  range: any;

  constructor(timeSrv: any, public functions: FuncInstance[], public client: any) {
    this.range = timeSrv.timeRange();
  }

  getStartTime() {
    return this.range.from.valueOf();
  }

  getEndTime() {
    return this.range.to.valueOf();
  }

  getNodeCriteria() {
    return this.getFirstParam('withExporterNode');
  }

  getInterfaceId() {
    return this.getFirstParam('withIfIndex');
  }

  getFirstParam(defName): any {
    let param = undefined;
    this.functions.forEach((func) => {
      if(func.def.name === defName) {
        param = func.params[0];
      }
    });
    return param;
  }
}
