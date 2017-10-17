'use strict';

System.register([], function (_export, _context) {
  "use strict";

  var _createClass, MemoEditorCtrl;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  /** @ngInject */
  function memoEditorAsDirective() {
    'use strict';

    return {
      restrict: 'E',
      templateUrl: 'public/plugins/opennms-helm-app/panels/alarm-table/memo_editor.html',
      controller: MemoEditorCtrl,
      scope: {
        alarm: '=',
        source: '=',
        type: '@'
      }
    };
  }

  _export('memoEditorAsDirective', memoEditorAsDirective);

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

      _export('MemoEditorCtrl', MemoEditorCtrl = function () {
        /** @ngInject */
        function MemoEditorCtrl($scope, datasourceSrv, timeSrv) {
          _classCallCheck(this, MemoEditorCtrl);

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
          var self = this;
          $scope.$watch('memoBody', function () {
            $scope.memoChanged = self.didMemoChange();
          });

          $scope.saveMemo = function () {
            $scope.actionInProgress = true;
            self.getDatasource().then(function (ds) {
              if ($scope.type === 'sticky') {
                return ds.saveSticky($scope.alarm.id, $scope.memoBody);
              } else {
                return ds.saveJournal($scope.alarm.id, $scope.memoBody);
              }
            }).then(function (res) {
              $scope.actionInProgress = false;
              self.refresh();
            }).catch(function (err) {
              $scope.actionInProgress = false;
              self.refresh();
            });
          };

          $scope.deleteMemo = function () {
            $scope.actionInProgress = true;
            self.getDatasource().then(function (ds) {
              if ($scope.type === 'sticky') {
                return ds.deleteSticky($scope.alarm.id);
              } else {
                return ds.deleteJournal($scope.alarm.id);
              }
            }).then(function (res) {
              $scope.actionInProgress = false;
              self.refresh();
            }).catch(function (err) {
              $scope.actionInProgress = false;
              self.refresh();
            });
          };
        }

        _createClass(MemoEditorCtrl, [{
          key: 'getDatasource',
          value: function getDatasource() {
            return this.datasourceSrv.get(this.$scope.source).then(function (ds) {
              if (ds.type && ds.type.indexOf("fault-datasource") < 0) {
                throw { message: 'Only OpenNMS datasources are supported' };
              } else {
                return ds;
              }
            });
          }
        }, {
          key: 'refresh',
          value: function refresh() {
            var self = this;
            this.getDatasource().then(function (ds) {
              return ds.getAlarm(self.$scope.alarmId);
            }).then(function (alarm) {
              self.setupWithAlarm(alarm);
            });
            // Refresh the dashboard
            self.timeSrv.refreshDashboard();
          }
        }, {
          key: 'setupWithAlarm',
          value: function setupWithAlarm(alarm) {
            this.$scope.alarm = alarm;
            this.$scope.memo = this.$scope.alarm[this.$scope.type];
            this.$scope.memoBody = this.$scope.memo ? this.$scope.memo.body : null;
            this.$scope.memoChanged = false;
          }
        }, {
          key: 'didMemoChange',
          value: function didMemoChange() {
            var originalMemoBody = null;
            if (this.$scope.memo) {
              originalMemoBody = this.$scope.memo.body;
            }
            return originalMemoBody !== this.$scope.memoBody;
          }
        }]);

        return MemoEditorCtrl;
      }());

      _export('MemoEditorCtrl', MemoEditorCtrl);
    }
  };
});
//# sourceMappingURL=memo_editor.js.map
