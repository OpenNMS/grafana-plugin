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
  constructor() {
      if (!this.current) {
      console.log('no current controller!');
      }
    if (!this.current.jsonData.timeout) {
      this.current.jsonData.timeout = 10;
    }
    this.patterns = {
      timeout: /^\d+$/
    };
  }
}

if (typeof angular !== 'undefined') {
  angular.module('grafana.directives')
    .directive('onmsTimeoutSettings', directive)
    .controller('OnmsTimeoutCtrl', OnmsTimeoutCtrl);
} else {
  console.warn('Angular not found!  <onms-timeout> will not work!');
}
