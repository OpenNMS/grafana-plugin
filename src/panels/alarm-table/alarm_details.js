import { TableRenderer } from "./renderer"
import md5 from 'crypto-js/md5';
import {Model} from 'opennms';
import _ from 'lodash';

const compareStrings = (a, b) => {
  return (a || b) ? (!a ? -1 : !b ? 1 : a.localeCompare(b)) : 0;
};

export class AlarmDetailsCtrl {
  /** @ngInject */
  constructor($scope, backendSrv, contextSrv, datasourceSrv) {
    this.$scope = $scope;
    this.backendSrv = backendSrv;
    this.contextSrv = contextSrv;
    this.datasourceSrv = datasourceSrv;
    this.editFeedback = false;
    $scope.editor = { index: 0 };

    this.CORRECT_OUTLINED = "btn feedback-button feedback-button";
    this.CORRECT_FILLED = "btn feedback-button feedback-button-correct";

    this.INCORRECT_OUTLINED = "btn feedback-button feedback-button";
    this.INCORRECT_FILLED = "btn feedback-button feedback-button-incorrect";

    this.ROOT_CAUSE_NO = "btn feedback-button feedback-button";
    this.ROOT_CAUSE_YES = "btn feedback-button feedback-button-root-cause";

    // Save the alarm
    $scope.alarm = $scope.$parent.alarm;
    $scope.source = $scope.$parent.source;

    if ($scope.alarm.relatedAlarms && $scope.alarm.relatedAlarms.length > 0) {
      const related = {};
      $scope.alarm.relatedAlarms.forEach(alarm => {
        const label = (alarm.nodeLabel === undefined || alarm.nodeLabel === null)? '' : alarm.nodeLabel;
        if (!related[label]) {
          related[label] = [];
        }
        related[label].push(alarm);
      });
      $scope.relatedAlarms = Object.keys(related).sort(compareStrings).map((label) => {
        return {
          label: label,
          alarms: related[label]
        };
      });
    }

    // Feedback Counts
    $scope.feedbackCorrectCount = 0;
    $scope.feedbackIncorrectCount = 0;

    // Compute the icon
    let severity = $scope.alarm.severity.label.toLowerCase();
    $scope.severityIcon = TableRenderer.getIconForSeverity(severity);
    $scope.severity = $scope.$parent.severity;

    // Situation Feedback
    $scope.situationFeebackEnabled = false;
    $scope.feebackButton = this.CORRECT_OUTLINED;

    // Compute the tabs
    $scope.tabs = ['Overview', 'Memos'];
    $scope.ticketingEnabled = $scope.$parent.ticketerConfig && $scope.$parent.ticketerConfig.enabled;
    if ($scope.ticketingEnabled) {
      $scope.tabs.push('Ticketing');
    }

    // Feedback Tags
    $scope.feedbackTags = new Set([]);

    // If this is a Situation, collect any correlation feedback previously submitted
    if ($scope.alarm.relatedAlarms && $scope.alarm.relatedAlarms.length > 0) {
      $scope.tabs.push('Related Alarms');
      let self = this;
      this.getDatasource().then(ds => { return ds.getSituationFeedback(self.$scope.alarm.id) })
        .then(
          function (feedback) {
            $scope.situationFeedback = self.initalizeFeeback();
            if (feedback && feedback.length > 0) {
              self.updateFeedback(feedback);
              $scope.hasSituationFeedback = true;
              $scope.retrievedFeedback = self.clone($scope.situationFeedback);
            }
            $scope.situationFeedbackButton = self.situationFeedbackButton();
            $scope.situationFeebackEnabled = true;
          })
        .catch(
          function (reason) {
            console.log("Situation Feedback not supported error: ", reason);
          });
    }

    $scope.tabs.push('JSON');
    $scope.getAlarmString = () => {
      return JSON.stringify($scope.alarm, undefined, 2);
    }

    // Raw global details link
    $scope.detailsLink = $scope.alarm.detailsPage.substring(0, $scope.alarm.detailsPage.indexOf("="));
  }

  // required to correctly re-assign values of the retrieved feedback to the working feedback
  clone(feedback) {
    let cloned  = [];
    for (var i = 0; i < feedback.length; i++) {
      let fb = new Model.OnmsSituationFeedback();
      fb.situationKey = feedback[i].situationKey;
      fb.situationFingerprint = feedback[i].situationFingerprint;
      fb.alarmKey = feedback[i].alarmKey;
      fb.feedbackType = feedback[i].feedbackType;
      fb.reason = feedback[i].reason;
      fb.rootCause = feedback[i].rootCause;
      fb.tags = feedback[i].tags;
      fb.user = feedback[i].user;
      cloned.push(fb);
    }
    return cloned;
  }

  detailFeedbackIncorrectButton(reductionKey) {
    let button = this.INCORRECT_OUTLINED;
    if (this.$scope.situationFeedback) {
      for (let feedback of this.$scope.situationFeedback) {
        if (feedback.alarmKey === reductionKey && feedback.feedbackType === Model.FeedbackTypes.FALSE_POSITIVE) {
          button = this.INCORRECT_FILLED;
          break;
        }
      }
    }
    return button;
  }

  detailFeedbackOkayButton(reductionKey) {
    let button = this.CORRECT_FILLED;
    if (this.$scope.situationFeedback) {
      for (let feedback of this.$scope.situationFeedback) {
        if (feedback.alarmKey === reductionKey && feedback.feedbackType === Model.FeedbackTypes.FALSE_POSITIVE) {
          button = this.CORRECT_OUTLINED;
          break;
        }
      }
    }
    return button;
  }

  detailFeedbackRootCauseButton(reductionKey) {
    let button = this.ROOT_CAUSE_NO;
    if (this.isRootCause(reductionKey)) {
      button = this.ROOT_CAUSE_YES;
    }
    return button;
  }

  initalizeFeeback() {
    this.$scope.feedbackCorrectCount = 0;
    this.$scope.feedbackIncorrectCount = 0;
    let feedback = [];
    for (let alarm of this.$scope.alarm.relatedAlarms) {
      let alarmFeedback = new Model.OnmsSituationFeedback();
      alarmFeedback.situationKey = this.$scope.alarm.reductionKey;
      alarmFeedback.situationFingerprint = this.fingerPrint(this.$scope.alarm);
      alarmFeedback.alarmKey = alarm.reductionKey;
      alarmFeedback.feedbackType = Model.FeedbackTypes.CORRECT;
      alarmFeedback.reason = "ALL_CORRECT";
      alarmFeedback.rootCause = false;
      alarmFeedback.tags = [];
      alarmFeedback.user = this.contextSrv.user.login;
      feedback.push(alarmFeedback);
      this.$scope.feedbackCorrectCount++;
    }
    return feedback;
  }

  isRootCause(reductionKey) {
    for (let feedback of this.$scope.situationFeedback) {
      if (feedback.alarmKey === reductionKey) {
        return feedback.rootCause;
      }
    }
    return false;
  }

  loadtags(prefix) {
    console.log("Load tags: " + prefix);
    return this.$scope.tagArray;
  }

  markIncorrect(reductionKey) {
    for (let feedback of this.$scope.situationFeedback) {
      if (feedback.alarmKey === reductionKey) {
        if (feedback.feedbackType == Model.FeedbackTypes.CORRECT) {
          feedback.feedbackType = Model.FeedbackTypes.FALSE_POSITIVE;
          this.$scope.feedbackCorrectCount--;
          this.$scope.feedbackIncorrectCount++;
          break;
        }
      }
    }
  }

  markCorrect(reductionKey) {
    for (let feedback of this.$scope.situationFeedback) {
      if (feedback.alarmKey === reductionKey && feedback.feedbackType === Model.FeedbackTypes.FALSE_POSITIVE) {
        feedback.feedbackType = Model.FeedbackTypes.CORRECT;
        this.$scope.feedbackCorrectCount++;
        this.$scope.feedbackIncorrectCount--;
        break;
      }
    }
  }

  markRootCause(reductionKey, wasRootCause) {
    let isRootCause = !wasRootCause; // marking or unmarking inverts the previous state
    for (let feedback of this.$scope.situationFeedback) {
      if (feedback.alarmKey === reductionKey) {
        feedback.rootCause = isRootCause;
      } else if (isRootCause) { // if we are making this alarm the root cause, all others are not
        feedback.rootCause = false;
      }
    }
  }

  resetCounters() {
    // reset the counters
    this.$scope.feedbackCorrectCount = this.$scope.situationFeedback.length;
    this.$scope.feedbackIncorrectCount = 0;
    for(let fb of this.$scope.situationFeedback) {
      if (fb.feedbackType === Model.FeedbackTypes.FALSE_POSITIVE) {
        this.$scope.feedbackCorrectCount--;
        this.$scope.feedbackIncorrectCount++;
      }
    }
  }

  submitEditedFeedback(form) {
    for (let feedback of this.$scope.situationFeedback) {
      feedback.tags = this.$scope.feedbackTags;
      if (form) {
        feedback.reason = form.reason;
      }
    }
    this.submitFeedback(this.$scope.situationFeedback);
  }

  submitFeedback(feedback) {
    let self = this;
    this.getDatasource().then(ds => { return ds.submitSituationFeedback(self.$scope.alarm.id, feedback) })
      .then(
        function () {
          self.$scope.editFeedback = false;
          self.$scope.submittedFeedback = true;
          self.$scope.hasSituationFeedback = true;
        })
      .catch(
        function (reason) {
          console.log("Got POST error: ", reason);
          self.$scope.editFeedback = false;
        });
  }

  updateFeedback(feedback) {
    for (let fb of feedback) {
      const index = this.$scope.situationFeedback.findIndex(ifb => ifb.alarmKey === fb.alarmKey);
      this.$scope.situationFeedback[index].rootCause = fb.rootCause;
      this.$scope.situationFeedback[index].tags = fb.tags;
      this.$scope.situationFeedback[index].feedbackType = fb.feedbackType;
      for (let tag of fb.tags) {
        this.$scope.feedbackTags.add(tag);
        $('#tags-input').tagsinput('add', tag);
      }
    }
    $('#tags-input').tagsinput('refresh');
  }

  editSituationFeedback() {
    this.$scope.editFeedback = true;
    this.$scope.submittedFeedback = false;
  }

  situationFeedbackButton() {
    let button = this.CORRECT_OUTLINED;
    let fingerprint = this.fingerPrint(this.$scope.alarm);
    if (this.$scope.situationFeedback) {
      for (let feedback of this.$scope.situationFeedback) {
        if (feedback.situationFingerprint == fingerprint && this.$scope.hasSituationFeedback) {
          button = this.CORRECT_FILLED;
          break;
        }
      }
    }
    return button;
  }

  cancelEditedFeedback() {
    this.$scope.situationFeedback = this.clone(this.$scope.retrievedFeedback);
    this.$scope.editFeedback = false;
    this.$scope.submittedFeedback = false;
    this.resetCounters();
  }

  fingerPrint(situation) {
    return btoa(md5(situation.relatedAlarms));
  }

  getDatasource() {
    return this.datasourceSrv.get(this.$scope.source).then(ds => {
      if (ds.type && ds.type.indexOf("fault-datasource") < 0) {
        throw { message: 'Only OpenNMS datasources are supported' };
      } else {
        return ds;
      }
    });
  }

  tagsTypeAhead(query) {
    // TODO - query rest endpoint for tags on the first time and then further filter them as typing continues
    // or hit rest endpoint each time...
    console.log("TYPEAHEAD: " + query);
  }

}

/** @ngInject */
export function alarmDetailsAsDirective() {
  'use strict';
  return {
    restrict: 'E',
    templateUrl: '/public/plugins/opennms-helm-app/panels/alarm-table/alarm_details.html',
    controller: AlarmDetailsCtrl,
    bindToController: true,
    controllerAs: 'ctrl',
    scope: { dismiss: "&" }
  };
}
