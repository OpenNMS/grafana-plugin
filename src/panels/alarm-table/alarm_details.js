
export class AlarmDetailsCtrl {

  /** @ngInject */
  constructor($scope) {
    this.$scope = $scope;
    $scope.editor = { index: 0 };
    $scope.alarm = $scope.$parent.alarm;
  }
}

/** @ngInject */
export function alarmDetailsAsDirective() {
  'use strict';
  return {
    restrict: 'E',
    templateUrl: 'public/plugins/opennms-helm-app/panels/alarm-table/alarm_details.html',
    controller: AlarmDetailsCtrl,
    bindToController: true,
    controllerAs: 'ctrl',
    scope: {dismiss: "&"}
  };
}