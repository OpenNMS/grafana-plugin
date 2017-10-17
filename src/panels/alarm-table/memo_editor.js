export class MemoEditorCtrl {
  /** @ngInject */
  constructor($scope, datasourceSrv, timeSrv) {
    this.$scope = $scope;
    this.datasourceSrv = datasourceSrv;
    this.timeSrv = timeSrv;

    // Require a valid type
    if ($scope.type !== 'journal' && $scope.type !== 'sticky') {
      throw "Unsupported memo type" + $scope.type;
    }

    // Save the id, this won't change during the lifetime of this controller
    $scope.alarmId = $scope.alarm.id;
    this.setupWithAlarm($scope.alarm);

    // Register a listener for the memo body so that we can enable the Save button
    // when the form is 'dirty'
    let self = this;
    $scope.$watch('memoBody', function() {
      $scope.memoChanged = self.didMemoChange();
    });

    $scope.saveMemo = function() {
      $scope.actionInProgress = true;
      self.getDatasource().then(ds => {
        if ($scope.type === 'sticky') {
          return ds.saveSticky($scope.alarm.id, $scope.memoBody);
        } else {
          return ds.saveJournal($scope.alarm.id, $scope.memoBody);
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

    $scope.deleteMemo = function() {
      $scope.actionInProgress = true;
      self.getDatasource().then(ds => {
        if ($scope.type === 'sticky') {
          return ds.deleteSticky($scope.alarm.id);
        } else {
          return ds.deleteJournal($scope.alarm.id);
        }
      })
      .then(res => {
        $scope.actionInProgress = false;
        self.refresh();
      })
      .catch(err => {
        $scope.actionInProgress = false;
        self.refresh();
      });
    };
  }

  getDatasource() {
    return this.datasourceSrv.get(this.$scope.source).then(ds => {
      if (ds.type && ds.type.indexOf("fault-datasource") < 0) {
        throw {message: 'Only OpenNMS datasources are supported'};
      } else {
        return ds;
      }
    });
  };

  refresh() {
    let self = this;
    this.getDatasource().then(ds => {return ds.getAlarm(self.$scope.alarmId)})
      .then(alarm => {
        self.setupWithAlarm(alarm)
      });
    // Refresh the dashboard
    self.timeSrv.refreshDashboard();
  }

  setupWithAlarm(alarm) {
    this.$scope.alarm = alarm;
    this.$scope.memo = this.$scope.alarm[this.$scope.type];
    this.$scope.memoBody = this.$scope.memo ? this.$scope.memo.body : null;
    this.$scope.memoChanged = false;
  }

  didMemoChange() {
    let originalMemoBody = null;
    if (this.$scope.memo) {
      originalMemoBody = this.$scope.memo.body;
    }
    return originalMemoBody !== this.$scope.memoBody;
  }
}

/** @ngInject */
export function memoEditorAsDirective() {
  'use strict';
  return {
    restrict: 'E',
    templateUrl: 'public/plugins/opennms-helm-app/panels/alarm-table/memo_editor.html',
    controller: MemoEditorCtrl,
    scope: {
      alarm: '=',
      source: '=',
      type: '@',
    }
  };
}
