'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.OpenNMSFMDatasourceQueryCtrl = undefined;

var _sdk = require('app/plugins/sdk');

require('./css/query-editor.css!');

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

var OpenNMSFMDatasourceQueryCtrl = exports.OpenNMSFMDatasourceQueryCtrl = function (_QueryCtrl) {
  _inherits(OpenNMSFMDatasourceQueryCtrl, _QueryCtrl);

  function OpenNMSFMDatasourceQueryCtrl($scope, $injector, $q) {
    _classCallCheck(this, OpenNMSFMDatasourceQueryCtrl);

    var _this = _possibleConstructorReturn(this, (OpenNMSFMDatasourceQueryCtrl.__proto__ || Object.getPrototypeOf(OpenNMSFMDatasourceQueryCtrl)).call(this, $scope, $injector));

    _this.q = $q;
    _this.scope = $scope;
    return _this;
  }

  return OpenNMSFMDatasourceQueryCtrl;
}(_sdk.QueryCtrl);

OpenNMSFMDatasourceQueryCtrl.templateUrl = 'datasources/mock-fault-ds/partials/query.editor.html';
//# sourceMappingURL=query_ctrl.js.map
