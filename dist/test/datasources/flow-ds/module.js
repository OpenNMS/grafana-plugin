'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.QueryOptionsCtrl = exports.ConfigCtrl = exports.QueryCtrl = exports.Datasource = undefined;

var _datasource = require('./datasource');

var _query_ctrl = require('./query_ctrl');

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var GenericConfigCtrl = function GenericConfigCtrl() {
  _classCallCheck(this, GenericConfigCtrl);
};

GenericConfigCtrl.templateUrl = 'datasources/flow-ds/partials/config.html';

var GenericQueryOptionsCtrl = function GenericQueryOptionsCtrl() {
  _classCallCheck(this, GenericQueryOptionsCtrl);
};

GenericQueryOptionsCtrl.templateUrl = 'datasources/flow-ds/partials/query.options.html';

exports.Datasource = _datasource.FlowDatasource;
exports.QueryCtrl = _query_ctrl.FlowDatasourceQueryCtrl;
exports.ConfigCtrl = GenericConfigCtrl;
exports.QueryOptionsCtrl = GenericQueryOptionsCtrl;
//# sourceMappingURL=module.js.map
