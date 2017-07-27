'use strict';

System.register(['./datasource', './query_ctrl'], function (_export, _context) {
  "use strict";

  var OpenNMSFMDatasource, OpenNMSFMDatasourceQueryCtrl, OpenNMSFMDatasourceConfigCtrl, OpenNMSFMDatasourceQueryOptionsCtrl;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  return {
    setters: [function (_datasource) {
      OpenNMSFMDatasource = _datasource.OpenNMSFMDatasource;
    }, function (_query_ctrl) {
      OpenNMSFMDatasourceQueryCtrl = _query_ctrl.OpenNMSFMDatasourceQueryCtrl;
    }],
    execute: function () {
      _export('ConfigCtrl', OpenNMSFMDatasourceConfigCtrl = function OpenNMSFMDatasourceConfigCtrl() {
        _classCallCheck(this, OpenNMSFMDatasourceConfigCtrl);
      });

      OpenNMSFMDatasourceConfigCtrl.templateUrl = 'datasources/fault-ds/partials/config.html';

      _export('QueryOptionsCtrl', OpenNMSFMDatasourceQueryOptionsCtrl = function OpenNMSFMDatasourceQueryOptionsCtrl() {
        _classCallCheck(this, OpenNMSFMDatasourceQueryOptionsCtrl);
      });

      OpenNMSFMDatasourceQueryOptionsCtrl.templateUrl = 'datasources/fault-ds/partials/query.options.html';

      _export('Datasource', OpenNMSFMDatasource);

      _export('QueryCtrl', OpenNMSFMDatasourceQueryCtrl);

      _export('ConfigCtrl', OpenNMSFMDatasourceConfigCtrl);

      _export('QueryOptionsCtrl', OpenNMSFMDatasourceQueryOptionsCtrl);
    }
  };
});
//# sourceMappingURL=module.js.map
