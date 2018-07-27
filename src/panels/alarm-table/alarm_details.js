import { TableRenderer } from "./renderer"

export class AlarmDetailsCtrl {

  /** @ngInject */
  constructor($scope, backendSrv, contextSrv, $q) {
    this.$scope = $scope;
    this.backendSrv = backendSrv;
    this.contextSrv = contextSrv;
    this.editFeedback = false;
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
    // If this is a Situation, collect any correlation feedback previously submitted
    if ($scope.alarm.relatedAlarms && $scope.alarm.relatedAlarms.length > 0) {
      $scope.tabs.push('Related Alarms');
      console.log($q);
      let request = this.doOpenNMSRequest({
        url: '/rest/situation-feedback/' + encodeURIComponent($scope.alarm.reductionKey),
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

  submitAllPositiveFeedback() {
    let self = this;
    let feedback = "[";
    for (let alarm of this.$scope.alarm.relatedAlarms) {
      feedback += this.alarmFeedback(this.$scope.alarm.reductionKey, this.fingerPrint(this.$scope.alarm), alarm, "CORRECT", "ALL_CORRECT", this.contextSrv.user.login);
      feedback += ", ";
    }
    feedback = feedback.replace(/,\s*$/, "");
    feedback += "]";
    let request = this.doOpenNMSRequest({
      url: '/rest/situation-feedback/' + encodeURIComponent(this.$scope.alarm.reductionKey),
      data: feedback,
      method: 'POST',
      headers: { 'Content-Type': 'application/json' }
    });
    request.then(
      function (response) {
        console.log("Got POST response: ", response);
        // TODO - test return value. on 204, display confirmation to the user.
        self.$scope.situationFeedback = response.data;
      });
  }

  editSituationFeedback() {
    this.$scope.editFeedback=true;
  }

  submitEditedFeedback() {
    console.log("TODO - submit Edited Feedback");
  }

  cancelEditedFeedback() {
    console.log("TODO - cancel Edited Feedback.");
  }

  fingerPrint(situation) {
    return "FIXME - implement fingerprint hash";
//    return this.md5.creatHash(situation.relatedAlarms);
  }

  alarmFeedback(situationKey, fingerprint, alarm, feedbackType, reason, user) {
    return "{\"situationKey\": \"" + situationKey + "\","
    + "\"situationFingerprint\": \"" + fingerprint + "\","
    + "\"alarmKey\": \"" + alarm.reductionKey + "\","
    + "\"feedbackType\": \"" + feedbackType + "\","
    + "\"reason\" : \"" + reason + "\","
    + "\"user\" : \"" + user + "\"}";
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
