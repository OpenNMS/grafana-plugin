'use strict';

System.register([], function (_export, _context) {
  "use strict";

  var _createClass, ModalCtrl;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  return {
    setters: [],
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

      ModalCtrl = function () {
        function ModalCtrl($scope) {
          _classCallCheck(this, ModalCtrl);

          this.$scope = $scope;
          this.query = "";
          this.selectedRow = null;

          this.searchForRows();
        }

        _createClass(ModalCtrl, [{
          key: 'searchForRows',
          value: function searchForRows() {
            var self = this;
            this.searching = true;
            this.$scope.search(this.query).then(function (results) {
              // Reset the selected row
              self.selectedRow = null;
              // Add the results to the scope
              self.rows = results.rows;
              self.count = results.count;
              self.totalCount = results.totalCount;
              // We're done
              self.searching = false;
            }, function () {
              self.searching = false;
            });
          }
        }, {
          key: 'setClickedRow',
          value: function setClickedRow(index) {
            if (this.selectedRow === index) {
              this.selectedRow = null;
            } else {
              this.selectedRow = index;
              // Keep a reference to the row when the selection is made
              this.row = this.rows[this.selectedRow];
            }
          }
        }, {
          key: 'cancel',
          value: function cancel() {
            this.$scope.result.reject();
            this.$scope.dismiss();
          }
        }, {
          key: 'ok',
          value: function ok() {
            if (this.selectedRow !== null) {
              this.$scope.result.resolve(this.row);
            } else {
              this.$scope.result.reject();
            }
            this.$scope.dismiss();
          }
        }]);

        return ModalCtrl;
      }();

      if (typeof angular !== 'undefined') {
        angular.module('grafana.controllers').controller('OpenNMSModalSelectionCtrl', ModalCtrl);
      }
    }
  };
});
//# sourceMappingURL=modal_ctrl.js.map
