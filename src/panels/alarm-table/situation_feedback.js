export class SituationFeedbackCtrl {
    /** @ngInject */
    constructor($scope, datasourceSrv, contextSrv, timeSrv) {
        this.$scope = $scope;
        this.datasourceSrv = datasourceSrv;
        this.contextSrv = contextSrv;
        this.timeSrv = timeSrv;

        $scope.alarmId = $scope.alarm.id;
        $scope.situationKey = $scope.alarm.reductionKey;

    $scope.submitFeedback = function() {
        $scope.actionInProgress = true;
        let user = this.contextSrv.user.login;
        self.getDatasource().then(ds => {
          if ($scope.feedbackType === 'correct') {
            return saveFeedback($scope.alarm.id, $scope.memoBody);
          } else {
            return saveFeedback($scope.alarm.id, $scope.memoBody);
          }
        })
        .then(res => {
          $scope.actionInProgress = false;
          self.refresh();
        })
        .catch(err => {
          $scope.actionInProgress = false;
          self.refresh();
        })
      };
    }
    
    saveFeedback() {
        // FIXME
    }

    refresh() {
        let self = this;
        this.getDatasource().then(ds => {return ds.getAlarm(self.$scope.alarmId)})
          .then(alarm => {
            self.setupWithAlarm(alarm)
          });
        // Refresh the dashboard
        self.timeSrv.refreshDashboard();
    }

}

/** @ngInject */
export function situationFeedbackAsDirective() {
    'use strict';
    return {
        restrict: 'E',
        templateUrl: 'public/plugins/opennms-helm-app/panels/alarm-table/situation_feedback.html',
        controller: SituationFeedbackCtrl,
        scope: {
            alarm: '=',
            source: '=',
            type: '@',
        }
    };
} 
