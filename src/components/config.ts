export class OpenNMSHelmAppConfigCtrl {
  appModel: any;
  data: any;
  static templateUrl: string;

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
    this.data.actions.push({ url: '' });
  }

  removeAction(actionIndex: number) {
    if (this.data.actions[actionIndex]) {
      console.info('removing action ' + this.data.actions[actionIndex].label + '(' + actionIndex + ')');
      this.data.actions.splice(actionIndex, 1);
    } else {
      console.warn('no action at index ' + actionIndex);
    }
  }
}

OpenNMSHelmAppConfigCtrl.templateUrl = 'public/plugins/opennms-helm-app/components/config.html';
