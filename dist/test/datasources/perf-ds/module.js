'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.QueryOptionsCtrl = exports.ConfigCtrl = exports.QueryCtrl = exports.Datasource = undefined;

var _datasource = require('./datasource');

var _query_ctrl = require('./query_ctrl');

var _sdk = require('app/plugins/sdk');

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var GenericConfigCtrl = function GenericConfigCtrl() {
  _classCallCheck(this, GenericConfigCtrl);
};

GenericConfigCtrl.templateUrl = 'datasources/perf-ds/partials/config.html';

var GenericQueryOptionsCtrl = function GenericQueryOptionsCtrl() {
  _classCallCheck(this, GenericQueryOptionsCtrl);
};

GenericQueryOptionsCtrl.templateUrl = 'datasources/perf-ds/partials/query.options.html';

(0, _sdk.loadPluginCss)({
  dark: 'plugins/opennms-helm/datasources/perf-ds/css/opennms.dark.css',
  light: 'plugins/opennms-helm/datasources/perf-ds/css/opennms.light.css'
});

exports.Datasource = _datasource.OpenNMSDatasource;
exports.QueryCtrl = _query_ctrl.OpenNMSQueryCtrl;
exports.ConfigCtrl = GenericConfigCtrl;
exports.QueryOptionsCtrl = GenericQueryOptionsCtrl;
//# sourceMappingURL=module.js.map
