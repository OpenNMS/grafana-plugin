'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.OpenNMSFMDatasourceQueryCtrl = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _sdk = require('app/plugins/sdk');

require('./css/query-editor.css!');

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

var _opennms = require('../../opennms');

var _Mapping = require('./Mapping');

var _UI = require('./UI');

require('./query-directive');

var _FilterCloner = require('./FilterCloner');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

var OpenNMSFMDatasourceQueryCtrl = exports.OpenNMSFMDatasourceQueryCtrl = function (_QueryCtrl) {
  _inherits(OpenNMSFMDatasourceQueryCtrl, _QueryCtrl);

  function OpenNMSFMDatasourceQueryCtrl($scope, $injector, $q, uiSegmentSrv) {
    _classCallCheck(this, OpenNMSFMDatasourceQueryCtrl);

    var _this = _possibleConstructorReturn(this, (OpenNMSFMDatasourceQueryCtrl.__proto__ || Object.getPrototypeOf(OpenNMSFMDatasourceQueryCtrl)).call(this, $scope, $injector));

    _this.$q = $q;
    _this.$scope = $scope;
    _this.uiSegmentSrv = uiSegmentSrv;
    _this.featuredAttributes = true; // limits the selection to the featured attributes
    _this.filterMapping = new _Mapping.Mapping.FilterMapping(_this.uiSegmentSrv);

    // The target filter may be de-serialized from persistence.
    // In order to re-initialize it properly, the filter is cloned.
    if (_this.target.filter) {
      _this.target.filter = new _FilterCloner.FilterCloner().cloneFilter(_this.target.filter);
    } else {
      _this.target.filter = new _opennms.API.Filter();
    }
    _this.uiFilter = _this.filterMapping.getUiFilter(_this.target.filter);
    return _this;
  }

  _createClass(OpenNMSFMDatasourceQueryCtrl, [{
    key: 'toggleEditorMode',
    value: function toggleEditorMode() {
      this.target.rawQuery = !this.target.rawQuery;
    }
  }, {
    key: 'onChangeInternal',
    value: function onChangeInternal() {
      this.panelCtrl.refresh(); // Asks the panel to refresh data.
    }
  }, {
    key: 'showClearRestrictions',
    value: function showClearRestrictions() {
      var query = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : this.uiFilter.query;

      var self = this;
      var booleanList = _lodash2.default.map(query.clauses, function (clause) {
        if (clause.restriction instanceof _UI.UI.Query) {
          return self.showClearRestrictions(clause.restriction);
        }
        return new _UI.UI.Controls.RemoveControl().filter(query, clause);
      });

      return _lodash2.default.reduce(booleanList, function (overall, current) {
        return overall || current;
      }, false);
    }
  }, {
    key: 'clearRestrictions',
    value: function clearRestrictions() {
      this.uiFilter.clear();
      this.uiFilter.updateControls();
      this.updateTargetFilter();
    }
  }, {
    key: 'updateTargetFilter',
    value: function updateTargetFilter() {
      this.target.filter = this.filterMapping.getApiFilter(this.uiFilter);
      this.panelCtrl.refresh();
    }
  }, {
    key: 'getCollapsedText',
    value: function getCollapsedText() {
      var collapsedText = this.uiFilter.getQueryString();
      return collapsedText;
    }
  }, {
    key: 'handleQueryError',
    value: function handleQueryError(err) {
      this.error = err.message || 'Failed to issue metric query';
      return [];
    }
  }]);

  return OpenNMSFMDatasourceQueryCtrl;
}(_sdk.QueryCtrl);

OpenNMSFMDatasourceQueryCtrl.templateUrl = 'datasources/fault-ds/partials/query.editor.html';
//# sourceMappingURL=query_ctrl.js.map
