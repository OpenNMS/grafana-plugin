import {QueryCtrl} from 'app/plugins/sdk';
import './css/query-editor.css!'
import _ from 'lodash';
import {API} from '../../opennms';
import {UI} from './ui';
import {Mapping} from './mapping';
import './query-directive'

export class OpenNMSFMDatasourceQueryCtrl extends QueryCtrl {

  constructor($scope, $injector, $q,uiSegmentSrv)  {
    super($scope, $injector);
    this.$q = $q;
    this.$scope = $scope;
    this.uiSegmentSrv = uiSegmentSrv;
    this.filterMapping = new Mapping.FilterMapping(this.uiSegmentSrv);

    // define and set up model
    this.target.filter = this.target.filter || new API.Filter();
    this.uiFilter = this.filterMapping.getUiFilter(this.target.filter);
  }

  toggleEditorMode() {
    this.target.rawQuery = !this.target.rawQuery;
  }

  onChangeInternal() {
    this.panelCtrl.refresh(); // Asks the panel to refresh data.
  }

    showClearRestrictions() {
      // if (this.uiFilter.getSize() == 1) {
      //     return this.uiFilter.table.getLastRow().getColumnCount() == 4;
      // } else if (this.uiFilter.getSize() > 1) {
      //     return true;
      // }
      return false;
    }

    clearRestrictions() {
      this.uiFilter.clear();
      this.uiFilter.query.updateControls();
      this.updateTargetFilter();
    }

  updateTargetFilter() {
      this.target.filter = this.filterMapping.getApiFilter(this.uiFilter);
      this.panelCtrl.refresh();
  }

    getCollapsedText() {
      var collapsedText = this.uiFilter.getQueryString();
      return collapsedText;
    }

  handleQueryError(err) {
      this.error = err.message || 'Failed to issue metric query';
      return [];
  }
}

OpenNMSFMDatasourceQueryCtrl.templateUrl = 'datasources/fault-ds/partials/query.editor.html';

