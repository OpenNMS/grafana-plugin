'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.FlowDatasourceQueryCtrl = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _sdk = require('app/plugins/sdk');

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

require('./add_opennms_func');

require('./func_editor');

var _flow_functions = require('./flow_functions');

require('./css/query-editor.css!');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

var FlowDatasourceQueryCtrl = exports.FlowDatasourceQueryCtrl = function (_QueryCtrl) {
  _inherits(FlowDatasourceQueryCtrl, _QueryCtrl);

  function FlowDatasourceQueryCtrl($scope, $injector, uiSegmentSrv) {
    _classCallCheck(this, FlowDatasourceQueryCtrl);

    var _this = _possibleConstructorReturn(this, (FlowDatasourceQueryCtrl.__proto__ || Object.getPrototypeOf(FlowDatasourceQueryCtrl)).call(this, $scope, $injector));

    _this.scope = $scope;
    _this.uiSegmentSrv = uiSegmentSrv;
    _this.parseTarget();
    return _this;
  }

  _createClass(FlowDatasourceQueryCtrl, [{
    key: 'parseTarget',
    value: function parseTarget() {
      this.segments = [];
      this.functions = [];
      this.error = null;

      if (this.target) {
        if (this.target.metric) {
          this.segments.push(this.uiSegmentSrv.getSegmentForValue(this.target.metric));
        }

        if (this.target.functions) {
          this.functions = _lodash2.default.map(this.target.functions, function (f) {
            var funcDef = _flow_functions.Gfuncs.getFuncDef(f.name);
            var func = _flow_functions.Gfuncs.createFuncInstance(funcDef);
            for (var i = 0; i < f.parameters.length; i++) {
              func.updateParam(f.parameters[i], i);
            }
            return func;
          });
        }
      }

      if (this.segments.length === 0) {
        this.segments.push(this.uiSegmentSrv.newSelectMetric());
      }
    }
  }, {
    key: 'updateModelTarget',
    value: function updateModelTarget() {
      this.target.metric = this.segments.length > 0 ? this.segments[0].value : undefined;
      this.target.functions = _lodash2.default.map(this.functions, function (f) {
        return f.render();
      });
    }
  }, {
    key: 'getAltSegments',
    value: function getAltSegments() {
      return Promise.resolve([{ value: 'conversations' }, { value: 'applications' }]);
    }
  }, {
    key: 'addFunction',
    value: function addFunction(funcDef) {
      var newFunc = _flow_functions.Gfuncs.createFuncInstance(funcDef, { withDefaultParams: true });
      newFunc.added = true;
      this.functions.push(newFunc);
      this.targetChanged();
    }
  }, {
    key: 'removeFunction',
    value: function removeFunction(func) {
      this.functions = _lodash2.default.without(this.functions, func);
      this.targetChanged();
    }
  }, {
    key: 'targetChanged',
    value: function targetChanged() {
      if (this.error) {
        return;
      }

      var oldTarget = this.target.target;
      this.updateModelTarget();

      if (this.target.target !== oldTarget) {
        var lastSegment = this.segments.length > 0 ? this.segments[this.segments.length - 1] : {};
        if (lastSegment.value !== 'select metric') {
          this.panelCtrl.refresh();
        }
      }

      this.refresh();
    }
  }, {
    key: 'refreshMetricData',
    value: function refreshMetricData() {
      this.panelCtrl.refresh(); // Asks the panel to refresh data.
    }
  }]);

  return FlowDatasourceQueryCtrl;
}(_sdk.QueryCtrl);

FlowDatasourceQueryCtrl.templateUrl = 'datasources/flow-ds/partials/query.editor.html';
//# sourceMappingURL=query_ctrl.js.map
