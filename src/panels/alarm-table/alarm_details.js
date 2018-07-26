import { TableRenderer } from "./renderer"

export class AlarmDetailsCtrl {

  /** @ngInject */
  constructor($scope, backendSrv, contextSrv, $q) {
    this.$scope = $scope;
    this.backendSrv = backendSrv;
    this.contextSrv = contextSrv;
    this.editFeedback = true;
    $scope.editor = { index: 0 };

    // Save the alarm
    $scope.alarm = $scope.$parent.alarm;
    $scope.source = $scope.$parent.source;

    // Compute the icon
    let severity = $scope.alarm.severity.label.toLowerCase();
    $scope.severityIcon = TableRenderer.getIconForSeverity(severity);

    // Compute the tabs
    $scope.tabs = ['Overview', 'Memos'];
    $scope.ticketingEnabled = $scope.$parent.ticketerConfig && $scope.$parent.ticketerConfig.enabled;
    if ($scope.ticketingEnabled) {
      $scope.tabs.push('Ticketing');
    }
    if ($scope.alarm.relatedAlarms && $scope.alarm.relatedAlarms.length > 0) {
      $scope.tabs.push('Related Alarms');
      console.log($q);
      let request = this.doOpenNMSRequest({
        //        url: '/rest/situation-feedback/' + encodeURIComponent($scope.alarm.reductionKey),
        url: '/rest/situation-feedback/' + encodeURIComponent('dasd'),
        // data: query,
        method: 'GET',
        headers: { 'Content-Type': 'application/json' }
      });
      request.then(
        function (response) {
          console.log("Got response: ", response);
          $scope.situationFeedback = response.data;
        });
    }

    // Raw global details link
    $scope.detailsLink = $scope.alarm.detailsPage.substring(0, $scope.alarm.detailsPage.indexOf("="));
  }

  submitPositiveFeedback() {
    let self = this;
    let query = "[{\"situationKey\": \"" + this.$scope.alarm.reductionKey + "\","
      + "\"situationFingerprint\": \"" + "mehPrint" + "\","
      + "\"alarmKey\": \"" + this.$scope.alarm.relatedAlarms[0].reductionKey + "\","
      + "\"feedbackType\": \"CORRECT\"," 
      + "\"reason\" : \"because\","
      + "\"user\" : \"" + this.contextSrv.user.login + "\"}]";
    let request = this.doOpenNMSRequest({
      url: '/rest/situation-feedback/' + encodeURIComponent(this.$scope.alarm.reductionKey),
      data: query,
      method: 'POST',
      headers: { 'Content-Type': 'application/json' }
    });
    request.then(
      function (response) {
        console.log("Got POST response: ", response);
        self.$scope.situationFeedback = response.data;
      });
  }

  doOpenNMSRequest(options) {
    options.withCredentials = true;
    options.headers = options.headers || {};
    options.headers.Authorization = 'Basic YWRtaW46YWRtaW4=';
    options.url = 'http://localhost:8980/opennms' + options.url;
    return this.backendSrv.datasourceRequest(options);
  };

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
    scope: { dismiss: "&" }
  };
}
