'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.QueryOptionsCtrl = exports.ConfigCtrl = exports.QueryCtrl = exports.Datasource = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _datasource = require('./datasource');

var _query_ctrl = require('./query_ctrl');

var _Examples = require('./Examples');

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var OpenNMSFMDatasourceConfigCtrl = function OpenNMSFMDatasourceConfigCtrl() {
  _classCallCheck(this, OpenNMSFMDatasourceConfigCtrl);
};

OpenNMSFMDatasourceConfigCtrl.templateUrl = 'datasources/fault-ds/partials/config.html';

var OpenNMSFMDatasourceQueryOptionsCtrl = function () {
  function OpenNMSFMDatasourceQueryOptionsCtrl(uiSegmentSrv) {
    _classCallCheck(this, OpenNMSFMDatasourceQueryOptionsCtrl);

    this.uiSegmentSrv = uiSegmentSrv;
    this.examples = _Examples.Examples;
  }

  _createClass(OpenNMSFMDatasourceQueryOptionsCtrl, [{
    key: 'createQueryFromExample',
    value: function createQueryFromExample(example) {
      var target = {
        isNew: true,
        filter: example.apiFilter
      };

      this.panelCtrl.panel.targets.push(target);
    }
  }]);

  return OpenNMSFMDatasourceQueryOptionsCtrl;
}();

OpenNMSFMDatasourceQueryOptionsCtrl.templateUrl = 'datasources/fault-ds/partials/query.options.html';

exports.Datasource = _datasource.OpenNMSFMDatasource;
exports.QueryCtrl = _query_ctrl.OpenNMSFMDatasourceQueryCtrl;
exports.ConfigCtrl = OpenNMSFMDatasourceConfigCtrl;
exports.QueryOptionsCtrl = OpenNMSFMDatasourceQueryOptionsCtrl;
//# sourceMappingURL=module.js.map
