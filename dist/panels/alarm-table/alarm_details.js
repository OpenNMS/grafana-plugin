'use strict';

System.register(['./renderer'], function (_export, _context) {
  "use strict";

  var TableRenderer, AlarmDetailsCtrl;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

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

  _export('alarmDetailsAsDirective', alarmDetailsAsDirective);

  return {
    setters: [function (_renderer) {
      TableRenderer = _renderer.TableRenderer;
    }],
    execute: function () {
      _export('AlarmDetailsCtrl', AlarmDetailsCtrl =

      /** @ngInject */
      function AlarmDetailsCtrl($scope) {
        _classCallCheck(this, AlarmDetailsCtrl);

        this.$scope = $scope;
        $scope.editor = { index: 0 };

        // Save the alarm
        $scope.alarm = $scope.$parent.alarm;
        $scope.source = $scope.$parent.source;

        // Compute the icon
        var severity = $scope.alarm.severity.label.toLowerCase();
        $scope.severityIcon = TableRenderer.getIconForSeverity(severity);

        // Compute the tabs
        $scope.tabs = ['Overview', 'Memos'];
        $scope.ticketingEnabled = $scope.$parent.ticketerConfig && $scope.$parent.ticketerConfig.enabled;
        if ($scope.ticketingEnabled) {
          $scope.tabs.push('Ticketing');
        }
        if ($scope.alarm.relatedAlarms && $scope.alarm.relatedAlarms.length > 0) {
          $scope.tabs.push('Related Alarms');
        }

        // Raw global details link
        $scope.detailsLink = $scope.alarm.detailsPage.substring(0, $scope.alarm.detailsPage.indexOf("="));
      });

      _export('AlarmDetailsCtrl', AlarmDetailsCtrl);
    }
  };
});
//# sourceMappingURL=alarm_details.js.map
