import {QueryCtrl} from 'app/plugins/sdk';
import './css/query-editor.css!'

export class OpenNMSFMDatasourceQueryCtrl extends QueryCtrl {

  constructor($scope, $injector, $q)  {
    super($scope, $injector);
    this.q = $q;
    this.scope = $scope;
  }
}

OpenNMSFMDatasourceQueryCtrl.templateUrl = 'datasources/mock-fault-ds/partials/query.editor.html';

