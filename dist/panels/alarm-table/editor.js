'use strict';

System.register(['lodash', 'jquery', 'moment', 'angular', './transformers', 'app/core/utils/kbn'], function (_export, _context) {
  "use strict";

  var _, $, moment, angular, transformers, kbn, _createClass, TablePanelEditorCtrl;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  /** @ngInject */
  function tablePanelEditor($q, uiSegmentSrv) {
    'use strict';

    return {
      restrict: 'E',
      scope: true,
      templateUrl: '/public/plugins/opennms-helm/panels/alarm-table/editor.html',
      controller: TablePanelEditorCtrl
    };
  }

  _export('tablePanelEditor', tablePanelEditor);

  return {
    setters: [function (_lodash) {
      _ = _lodash.default;
    }, function (_jquery) {
      $ = _jquery.default;
    }, function (_moment) {
      moment = _moment.default;
    }, function (_angular) {
      angular = _angular.default;
    }, function (_transformers) {
      transformers = _transformers.transformers;
    }, function (_appCoreUtilsKbn) {
      kbn = _appCoreUtilsKbn.default;
    }],
    execute: function () {
      _createClass = function () {
        function defineProperties(target, props) {
          for (var i = 0; i < props.length; i++) {
            var descriptor = props[i];
            descriptor.enumerable = descriptor.enumerable || false;
            descriptor.configurable = true;
            if ("value" in descriptor) descriptor.writable = true;
            Object.defineProperty(target, descriptor.key, descriptor);
          }
        }

        return function (Constructor, protoProps, staticProps) {
          if (protoProps) defineProperties(Constructor.prototype, protoProps);
          if (staticProps) defineProperties(Constructor, staticProps);
          return Constructor;
        };
      }();

      _export('TablePanelEditorCtrl', TablePanelEditorCtrl = function () {
        /** @ngInject */
        function TablePanelEditorCtrl($scope, $q, uiSegmentSrv) {
          _classCallCheck(this, TablePanelEditorCtrl);

          this.$q = $q;
          this.uiSegmentSrv = uiSegmentSrv;
          $scope.editor = this;
          this.panelCtrl = $scope.ctrl;
          this.panel = this.panelCtrl.panel;
          this.transformers = transformers;
          this.fontSizes = ['80%', '90%', '100%', '110%', '120%', '130%', '150%', '160%', '180%', '200%', '220%', '250%'];

          this.addColumnSegment = uiSegmentSrv.newPlusButton();
        }

        _createClass(TablePanelEditorCtrl, [{
          key: 'getColumnOptions',
          value: function getColumnOptions() {
            var _this = this;

            if (!this.panelCtrl.dataRaw) {
              return this.$q.when([]);
            }
            var columns = this.transformers[this.panel.transform].getColumns(this.panelCtrl.dataRaw);
            // Filter out columns that have already been selected
            var self = this;
            columns = columns.filter(function (a) {
              return self.panel.columns.indexOf(a) < 0;
            });
            var segments = _.map(columns, function (c) {
              return _this.uiSegmentSrv.newSegment({ value: c.text });
            });
            return this.$q.when(segments);
          }
        }, {
          key: 'addColumn',
          value: function addColumn() {
            var columns = transformers[this.panel.transform].getColumns(this.panelCtrl.dataRaw);
            var column = _.find(columns, { text: this.addColumnSegment.value });

            if (column) {
              this.panel.columns.push(column);
              this.render();
            }

            var plusButton = this.uiSegmentSrv.newPlusButton();
            this.addColumnSegment.html = plusButton.html;
            this.addColumnSegment.value = plusButton.value;
          }
        }, {
          key: 'transformChanged',
          value: function transformChanged() {
            this.panel.columns = [];
            if (this.panel.transform === 'timeseries_aggregations') {
              this.panel.columns.push({ text: 'Avg', value: 'avg' });
            }

            this.render();
          }
        }, {
          key: 'render',
          value: function render() {
            this.panelCtrl.render();
          }
        }, {
          key: 'removeColumn',
          value: function removeColumn(column) {
            this.panel.columns = _.without(this.panel.columns, column);
            this.panelCtrl.render();
          }
        }]);

        return TablePanelEditorCtrl;
      }());

      _export('TablePanelEditorCtrl', TablePanelEditorCtrl);
    }
  };
});
//# sourceMappingURL=editor.js.map
