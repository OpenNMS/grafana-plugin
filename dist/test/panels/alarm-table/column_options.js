'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ColumnOptionsCtrl = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

exports.columnOptionsTab = columnOptionsTab;

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

var _jquery = require('jquery');

var _jquery2 = _interopRequireDefault(_jquery);

var _moment = require('moment');

var _moment2 = _interopRequireDefault(_moment);

var _angular = require('angular');

var _angular2 = _interopRequireDefault(_angular);

var _kbn = require('app/core/utils/kbn');

var _kbn2 = _interopRequireDefault(_kbn);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var ColumnOptionsCtrl = exports.ColumnOptionsCtrl = function () {
  /** @ngInject */
  function ColumnOptionsCtrl($scope, $q, uiSegmentSrv) {
    var _this = this;

    _classCallCheck(this, ColumnOptionsCtrl);

    this.$q = $q;
    this.uiSegmentSrv = uiSegmentSrv;
    $scope.editor = this;
    this.activeStyleIndex = 0;
    this.panelCtrl = $scope.ctrl;
    this.panel = this.panelCtrl.panel;
    this.unitFormats = _kbn2.default.getUnitFormats();
    this.colorModes = [{ text: 'Disabled', value: null }, { text: 'Cell', value: 'cell' }, { text: 'Value', value: 'value' }, { text: 'Row', value: 'row' }];
    this.columnTypes = [{ text: 'Number', value: 'number' }, { text: 'String', value: 'string' }, { text: 'Date', value: 'date' }, { text: 'Hidden', value: 'hidden' }];
    this.fontSizes = ['80%', '90%', '100%', '110%', '120%', '130%', '150%', '160%', '180%', '200%', '220%', '250%'];
    this.dateFormats = [{ text: 'DD MMM HH:mm:ss', value: 'DD MMM HH:mm:ss' }, { text: 'YYYY-MM-DD HH:mm:ss', value: 'YYYY-MM-DD HH:mm:ss' }, { text: 'MM/DD/YY h:mm:ss a', value: 'MM/DD/YY h:mm:ss a' }, { text: 'MMMM D, YYYY LT', value: 'MMMM D, YYYY LT' }];

    this.getColumnNames = function () {
      if (!_this.panelCtrl.table) {
        return [];
      }
      return _lodash2.default.map(_this.panelCtrl.table.columns, function (col) {
        return col.text;
      });
    };
  }

  _createClass(ColumnOptionsCtrl, [{
    key: 'render',
    value: function render() {
      this.panelCtrl.render();
    }
  }, {
    key: 'setUnitFormat',
    value: function setUnitFormat(column, subItem) {
      column.unit = subItem.value;
      this.panelCtrl.render();
    }
  }, {
    key: 'addColumnStyle',
    value: function addColumnStyle() {
      var newStyleRule = {
        unit: 'short',
        type: 'number',
        alias: '',
        decimals: 2,
        colors: ["rgba(245, 54, 54, 0.9)", "rgba(237, 129, 40, 0.89)", "rgba(50, 172, 45, 0.97)"],
        colorMode: null,
        pattern: '',
        dateFormat: 'YYYY-MM-DD HH:mm:ss',
        thresholds: [],
        width: "",
        clip: false
      };

      var styles = this.panel.styles;
      var stylesCount = styles.length;
      var indexToInsert = stylesCount;

      // check if last is a catch all rule, then add it before that one
      if (stylesCount > 0) {
        var last = styles[stylesCount - 1];
        if (last.pattern === '/.*/') {
          indexToInsert = stylesCount - 1;
        }
      }

      styles.splice(indexToInsert, 0, newStyleRule);
      this.activeStyleIndex = indexToInsert;
    }
  }, {
    key: 'removeColumnStyle',
    value: function removeColumnStyle(style) {
      this.panel.styles = _lodash2.default.without(this.panel.styles, style);
    }
  }, {
    key: 'invertColorOrder',
    value: function invertColorOrder(index) {
      var ref = this.panel.styles[index].colors;
      var copy = ref[0];
      ref[0] = ref[2];
      ref[2] = copy;
      this.panelCtrl.render();
    }
  }]);

  return ColumnOptionsCtrl;
}();

/** @ngInject */


function columnOptionsTab($q, uiSegmentSrv) {
  'use strict';

  return {
    restrict: 'E',
    scope: true,
    templateUrl: 'public/plugins/opennms-helm-app/panels/alarm-table/column_options.html',
    controller: ColumnOptionsCtrl
  };
}
//# sourceMappingURL=column_options.js.map
