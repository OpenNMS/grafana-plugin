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
      let self = this;
      request.then(
        function (response) {
          console.log("Got response: ", response);
          // FIXME - if no data - initialize - but don't fill the AllThumbsUp.
          if (response.data && response.data.length > 0) {
            $scope.situationFeedback = response.data;
          } else {
            $scope.situationFeedback = self.initalizeFeeback();
          }
          $scope.situationFeedbackOkayButton = self.situationFeedbackOkayButton();
        });
    }

    // Raw global details link
    $scope.detailsLink = $scope.alarm.detailsPage.substring(0, $scope.alarm.detailsPage.indexOf("="));
  }

  buttonFor(reductionKey) {
    let feedback = this.$scope.situationFeedback;
    if (feedback == null) {
      return "[meh]";
    }
    for (let alarmFeedback of feedback) {
      // TODO - see if this alarm is in the Feedback list with current fingerprint.
    }
  }

  detailFeedbackIncorrectButton(reductionKey) {
    let button = "/public/plugins/opennms-helm-app/img/thumbs-down";
    for (let feedback of this.$scope.situationFeedback) {
      if (feedback.alarmKey == reductionKey && feedback.feedbackType == "FALSE_POSITVE") {
        button += "-fill";
        break;
      }
    }
    button += ".png";
    return button;
  }

  detailFeedbackOkayButton(reductionKey) {
    let button = "/public/plugins/opennms-helm-app/img/thumb-up-filled";
    for (let feedback of this.$scope.situationFeedback) {
      if (feedback.alarmKey == reductionKey && feedback.feedbackType == "FALSE_POSITVE") {
        button = "/public/plugins/opennms-helm-app/img/thumb-up";
        break;
      }
    }
    button += ".png";
    return button;
  }

  initalizeFeeback() {
    let feedback = "[";
    for (let alarm of this.$scope.alarm.relatedAlarms) {
      feedback += this.alarmFeedback(this.$scope.alarm.reductionKey, this.fingerPrint(this.$scope.alarm), alarm, "CORRECT", "ALL_CORRECT", this.contextSrv.user.login);
      feedback += ", ";
    }
    feedback = feedback.replace(/,\s*$/, "");
    feedback += "]";
    return feedback;
  }

  negativeFeedback(reductionKey) {
    // Set FEEDBACK type on this.
    console.log("Negative Feedback for  " + reductionKey)
    for (let feedback of this.$scope.situationFeedback) {
      if (feedback.alarmKey == reductionKey) {
        feedback.feedbackType = "FALSE_POSITVE";
        console.log("Marked " + reductionKey + "as incorrect.")
        break;
      }
    }
  }

  positiveFeedback(reductionKey) {
    console.log("Positive Feedback for  " + reductionKey)
    // Set FEEDBACK type on this.
    for (let feedback of this.$scope.situationFeedback) {
      if (feedback.alarmKey == reductionKey) {
        feedback.feedbackType = "CORRECT";
        console.log("Marked " + reductionKey + "as correct.")
        break;
      }
    }
  }


  submitAllPositiveFeedback() {
    let self = this;
    let feedback = this.initalizeFeeback();
    console.log("submit all positive Feedback");
    this.submitFeedback(feedback);
  }

  submitEditedFeedback() {
    console.log("submit Edited Feedback");
    this.submitFeedback(this.$scope.situationFeedback);
  }

  submitFeedback(feedback) {
    console.log("Submitting Feedback: " + feedback);
    let self = this;
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
        self.$scope.editFeedback = false;
        self.$scope.submittedFeedback = true;
      });
  
  }

  editSituationFeedback() {
    this.$scope.editFeedback = true;
    this.$scope.submittedFeedback = false;
  }

  situationFeedbackOkayButton() {
    let button = "/public/plugins/opennms-helm-app/img/thumb-up";
    let fingerprint = this.fingerPrint(this.$scope.alarm);
    for (let feedback of this.$scope.situationFeedback) {
      if (feedback.situationFingerprint == fingerprint) {
        button += "-filled";
        break;
      }
    }
    button += ".png";
    return button;
  }

  cancelEditedFeedback() {
    this.$scope.editFeedback = false;
    this.$scope.submittedFeedback = false;
  }

  fingerPrint(situation) {
    return "FIXME - implement fingerprint hash";
    //    return this.md5.creatHash(situation.relatedAlarms);
  }

  newFeedback(situationKey, fingerprint, alarmKey, feedbackType, reason, user) {
    let feedback;
    feedback.situationKey = situationKey;
    feedback.fingerprint = fingerprint;
    feedback.alarmKey = alarmKey;
    feedback.feedbackType = feedbackType;
    feedback.reason = reason;
    feedback.user = user;
    return feedback;
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
