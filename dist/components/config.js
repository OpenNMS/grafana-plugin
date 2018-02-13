'use strict';

System.register(['lodash'], function (_export, _context) {
  "use strict";

  var _, _createClass, OpenNMSHelmAppConfigCtrl;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  return {
    setters: [function (_lodash) {
      _ = _lodash.default;
    }],
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

      _export('OpenNMSHelmAppConfigCtrl', OpenNMSHelmAppConfigCtrl = function () {
        /** @ngInject */
        function OpenNMSHelmAppConfigCtrl() {
          _classCallCheck(this, OpenNMSHelmAppConfigCtrl);

          if (!this.appModel.jsonData) {
            this.appModel.jsonData = {};
          }
          if (!this.appModel.jsonData.actions) {
            this.appModel.jsonData.actions = [];
          }
          this.data = this.appModel.jsonData;
        }

        _createClass(OpenNMSHelmAppConfigCtrl, [{
          key: 'addAction',
          value: function addAction() {
            this.data.actions.push({ url: '' });
          }
        }, {
          key: 'removeAction',
          value: function removeAction(actionIndex) {
            if (this.data.actions[actionIndex]) {
              console.log('removing action ' + this.data.actions[actionIndex].label + '(' + actionIndex + ')');
              this.data.actions.splice(actionIndex, 1);
            } else {
              console.warn('no action at index ' + actionIndex);
            }
          }
        }]);

        return OpenNMSHelmAppConfigCtrl;
      }());

      _export('OpenNMSHelmAppConfigCtrl', OpenNMSHelmAppConfigCtrl);

      OpenNMSHelmAppConfigCtrl.templateUrl = 'components/config.html';
    }
  };
});
//# sourceMappingURL=config.js.map
