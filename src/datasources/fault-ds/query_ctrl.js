import {QueryCtrl} from 'app/plugins/sdk';
import './css/query-editor.css!'

export class OpenNMSFMDatasourceQueryCtrl extends QueryCtrl {

  constructor($scope, $injector, $q)  {
    super($scope, $injector);
    this.q = $q;
    this.scope = $scope;
  }

  toggleEditorMode() {
    this.target.rawQuery = !this.target.rawQuery;
  }

  onChangeInternal() {
    this.panelCtrl.refresh(); // Asks the panel to refresh data.
  }
}

OpenNMSFMDatasourceQueryCtrl.templateUrl = 'datasources/fault-ds/partials/query.editor.html';

