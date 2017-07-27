'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.alarmDetailsAsDirective = alarmDetailsAsDirective;

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var AlarmDetailsCtrl =

/** @ngInject */
exports.AlarmDetailsCtrl = function AlarmDetailsCtrl($scope) {
  _classCallCheck(this, AlarmDetailsCtrl);

  this.$scope = $scope;
  $scope.editor = { index: 0 };
  $scope.alarm = $scope.$parent.alarm;
};

/** @ngInject */


function alarmDetailsAsDirective() {
  'use strict';

  return {
    restrict: 'E',
    templateUrl: 'public/plugins/opennms-helm-app/panels/alarm-table/alarm_details.html',
    controller: AlarmDetailsCtrl,
    bindToController: true,
    controllerAs: 'ctrl',
    scope: { dismiss: "&" }
  };
}
//# sourceMappingURL=alarm_details.js.map
