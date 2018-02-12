import _ from 'lodash';

export class OpenNMSHelmAppConfigCtrl {
  /** @ngInject */
  constructor() {
    if (!this.appModel.jsonData) {
      this.appModel.jsonData = {};
    }
    if (!this.appModel.jsonData.actions) {
      this.appModel.jsonData.actions = [];
    }
    this.data = this.appModel.jsonData;
  }

  addAction() {
    this.data.actions.push({url:''});
  }

  removeAction(actionIndex) {
    if (this.data.actions[actionIndex]) {
      console.log('removing action ' + this.data.actions[actionIndex].label + '(' + actionIndex + ')');
      this.data.actions.splice(actionIndex,1);
    } else {
      console.warn('no action at index ' + actionIndex);
    }
  }
}

OpenNMSHelmAppConfigCtrl.templateUrl = 'components/config.html';
