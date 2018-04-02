'use strict';

System.register(['./datasource', './query_ctrl'], function (_export, _context) {
  "use strict";

  var FlowDatasource, FlowDatasourceQueryCtrl, GenericConfigCtrl, GenericQueryOptionsCtrl;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  return {
    setters: [function (_datasource) {
      FlowDatasource = _datasource.FlowDatasource;
    }, function (_query_ctrl) {
      FlowDatasourceQueryCtrl = _query_ctrl.FlowDatasourceQueryCtrl;
    }],
    execute: function () {
      _export('ConfigCtrl', GenericConfigCtrl = function GenericConfigCtrl() {
        _classCallCheck(this, GenericConfigCtrl);
      });

      GenericConfigCtrl.templateUrl = 'datasources/flow-ds/partials/config.html';

      _export('QueryOptionsCtrl', GenericQueryOptionsCtrl = function GenericQueryOptionsCtrl() {
        _classCallCheck(this, GenericQueryOptionsCtrl);
      });

      GenericQueryOptionsCtrl.templateUrl = 'datasources/flow-ds/partials/query.options.html';

      _export('Datasource', FlowDatasource);

      _export('QueryCtrl', FlowDatasourceQueryCtrl);

      _export('ConfigCtrl', GenericConfigCtrl);

      _export('QueryOptionsCtrl', GenericQueryOptionsCtrl);
    }
  };
});
//# sourceMappingURL=module.js.map
