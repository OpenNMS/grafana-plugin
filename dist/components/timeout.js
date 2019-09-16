'use strict';

System.register([], function (_export, _context) {
  "use strict";

  var directive, OnmsTimeoutCtrl;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  return {
    setters: [],
    execute: function () {
      directive = function directive() {
        return {
          templateUrl: 'public/plugins/opennms-helm-app/components/timeout.html',
          restrict: 'E',
          controller: 'OnmsTimeoutCtrl',
          controllerAs: 'ctrl',
          bindToController: true,
          scope: {
            current: '='
          }
        };
      };

      OnmsTimeoutCtrl = function OnmsTimeoutCtrl() {
        _classCallCheck(this, OnmsTimeoutCtrl);

        if (!this.current) {
          console.log('no current controller!');
        }
        if (!this.current.jsonData.timeout) {
          this.current.jsonData.timeout = 10;
        }
        this.patterns = {
          timeout: /^\d+$/
        };
      };

      if (typeof angular !== 'undefined') {
        angular.module('grafana.directives').directive('onmsTimeoutSettings', directive).controller('OnmsTimeoutCtrl', OnmsTimeoutCtrl);
      } else {
        console.warn('Angular not found!  <onms-timeout> will not work!');
      }
    }
  };
});
//# sourceMappingURL=timeout.js.map
