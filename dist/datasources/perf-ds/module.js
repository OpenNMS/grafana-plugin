'use strict';

System.register(['./datasource', './query_ctrl', 'app/plugins/sdk'], function (_export, _context) {
  "use strict";

  var OpenNMSDatasource, OpenNMSQueryCtrl, loadPluginCss, GenericConfigCtrl, GenericQueryOptionsCtrl;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  return {
    setters: [function (_datasource) {
      OpenNMSDatasource = _datasource.OpenNMSDatasource;
    }, function (_query_ctrl) {
      OpenNMSQueryCtrl = _query_ctrl.OpenNMSQueryCtrl;
    }, function (_appPluginsSdk) {
      loadPluginCss = _appPluginsSdk.loadPluginCss;
    }],
    execute: function () {
      _export('ConfigCtrl', GenericConfigCtrl = function GenericConfigCtrl() {
        _classCallCheck(this, GenericConfigCtrl);
      });

      GenericConfigCtrl.templateUrl = 'datasources/perf-ds/partials/config.html';

      _export('QueryOptionsCtrl', GenericQueryOptionsCtrl = function GenericQueryOptionsCtrl() {
        _classCallCheck(this, GenericQueryOptionsCtrl);
      });

      GenericQueryOptionsCtrl.templateUrl = 'datasources/perf-ds/partials/query.options.html';

      loadPluginCss({
        dark: 'plugins/opennms-helm-app/datasources/perf-ds/css/opennms.dark.css',
        light: 'plugins/opennms-helm-app/datasources/perf-ds/css/opennms.light.css'
      });

      _export('Datasource', OpenNMSDatasource);

      _export('QueryCtrl', OpenNMSQueryCtrl);

      _export('ConfigCtrl', GenericConfigCtrl);

      _export('QueryOptionsCtrl', GenericQueryOptionsCtrl);
    }
  };
});
//# sourceMappingURL=module.js.map
