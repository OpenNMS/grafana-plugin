'use strict';

System.register(['./datasource', './query_ctrl', './Examples', '../../components/timeout'], function (_export, _context) {
  "use strict";

  var OpenNMSFMDatasource, OpenNMSFMDatasourceQueryCtrl, Examples, _createClass, OpenNMSFMDatasourceConfigCtrl, OpenNMSFMDatasourceQueryOptionsCtrl;

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
    }, function (_Examples) {
      Examples = _Examples.Examples;
    }, function (_componentsTimeout) {}],
    execute: function () {
      _createClass = function () {
        function defineProperties(target, props) {
          for (var i = 0; i < props.length; i++) {
            var descriptor = props[i];
            descriptor.enumerable = descriptor.enumerable || false;
            descriptor.configurable = true;
            if ("value" in descriptor) descriptor.writable = true;
            Object.defineProperty(target, descriptor.key, descriptor);
          }
        }

        return function (Constructor, protoProps, staticProps) {
          if (protoProps) defineProperties(Constructor.prototype, protoProps);
          if (staticProps) defineProperties(Constructor, staticProps);
          return Constructor;
        };
      }();

      _export('ConfigCtrl', OpenNMSFMDatasourceConfigCtrl = function OpenNMSFMDatasourceConfigCtrl() {
        _classCallCheck(this, OpenNMSFMDatasourceConfigCtrl);
      });

      OpenNMSFMDatasourceConfigCtrl.templateUrl = 'datasources/fault-ds/partials/config.html';

      _export('QueryOptionsCtrl', OpenNMSFMDatasourceQueryOptionsCtrl = function () {
        function OpenNMSFMDatasourceQueryOptionsCtrl(uiSegmentSrv) {
          _classCallCheck(this, OpenNMSFMDatasourceQueryOptionsCtrl);

          this.uiSegmentSrv = uiSegmentSrv;
          this.examples = Examples;
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
      }());

      OpenNMSFMDatasourceQueryOptionsCtrl.templateUrl = 'datasources/fault-ds/partials/query.options.html';

      _export('Datasource', OpenNMSFMDatasource);

      _export('QueryCtrl', OpenNMSFMDatasourceQueryCtrl);

      _export('ConfigCtrl', OpenNMSFMDatasourceConfigCtrl);

      _export('QueryOptionsCtrl', OpenNMSFMDatasourceQueryOptionsCtrl);
    }
  };
});
//# sourceMappingURL=module.js.map
