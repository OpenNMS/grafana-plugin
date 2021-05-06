const directive = () => {
  return {
    templateUrl: 'public/plugins/opennms-helm-app/components/timeout.html',
    restrict: 'E',
    controller: 'OnmsTimeoutCtrl',
    controllerAs: 'ctrl',
    bindToController: true,
    scope: {
      current: '='
    }
  };
};

class OnmsTimeoutCtrl {
  current: any;
  patterns: { [key: string]: RegExp };

  /** @ngInject */
  constructor() {
    this.patterns = {
      timeout: /^\d+$/
    };

    if (!this.current) {
      console.warn('no current controller!');
      return;
    }

    if (!this.current.jsonData) {
      this.current.jsonData = {};
    }
    if (!this.current.jsonData.timeout) {
      this.current.jsonData.timeout = 10;
    }
  }
}

if (typeof angular !== 'undefined') {
  const injector = angular.injector();
  if (!injector.has('onmstimeoutSettingsDirective')) {
    // only register it once
    angular.module('grafana.directives')
    .directive('onmsTimeoutSettings', directive)
    .controller('OnmsTimeoutCtrl', OnmsTimeoutCtrl);
  }
} else {
  console.warn('Angular not found!  <onms-timeout> will not work!');
}
