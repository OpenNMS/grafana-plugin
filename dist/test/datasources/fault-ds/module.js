'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.QueryOptionsCtrl = exports.ConfigCtrl = exports.QueryCtrl = exports.Datasource = undefined;

var _datasource = require('./datasource');

var _query_ctrl = require('./query_ctrl');

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var OpenNMSFMDatasourceConfigCtrl = function OpenNMSFMDatasourceConfigCtrl() {
  _classCallCheck(this, OpenNMSFMDatasourceConfigCtrl);
};

OpenNMSFMDatasourceConfigCtrl.templateUrl = 'datasources/fault-ds/partials/config.html';

var OpenNMSFMDatasourceQueryOptionsCtrl = function OpenNMSFMDatasourceQueryOptionsCtrl() {
  _classCallCheck(this, OpenNMSFMDatasourceQueryOptionsCtrl);
};

OpenNMSFMDatasourceQueryOptionsCtrl.templateUrl = 'datasources/fault-ds/partials/query.options.html';

exports.Datasource = _datasource.OpenNMSFMDatasource;
exports.QueryCtrl = _query_ctrl.OpenNMSFMDatasourceQueryCtrl;
exports.ConfigCtrl = OpenNMSFMDatasourceConfigCtrl;
exports.QueryOptionsCtrl = OpenNMSFMDatasourceQueryOptionsCtrl;
//# sourceMappingURL=module.js.map
