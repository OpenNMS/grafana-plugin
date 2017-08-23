import {QueryCtrl} from 'app/plugins/sdk';
import './css/query-editor.css!'
import _ from 'lodash';
import {API} from '../../opennms';
import {Mapping} from './Mapping';
import {UI} from './UI';
import './query-directive'
import {FilterCloner} from "./FilterCloner";

export class OpenNMSFMDatasourceQueryCtrl extends QueryCtrl {

  constructor($scope, $injector, $q,uiSegmentSrv)  {
    super($scope, $injector);
    this.$q = $q;
    this.$scope = $scope;
    this.uiSegmentSrv = uiSegmentSrv;
    this.featuredAttributes = true; // limits the selection to the featured attributes
    this.filterMapping = new Mapping.FilterMapping(this.uiSegmentSrv);

    // The target filter may be de-serialized from persistence.
    // In order to re-initialize it properly, the filter is cloned.
    if (this.target.filter) {
      this.target.filter = new FilterCloner().cloneFilter(this.target.filter);
    } else {
      this.target.filter = new API.Filter();
    }
    this.uiFilter = this.filterMapping.getUiFilter(this.target.filter);
  }

  toggleEditorMode() {
    this.target.rawQuery = !this.target.rawQuery;
  }

  onChangeInternal() {
    this.panelCtrl.refresh(); // Asks the panel to refresh data.
  }

    showClearRestrictions(query = this.uiFilter.query) {
      const self = this;
      const booleanList = _.map(query.clauses, clause => {
        if (clause.restriction instanceof UI.Query) {
          return self.showClearRestrictions(clause.restriction);
        }
        return new UI.Controls.RemoveControl().filter(query, clause);
      });

      return _.reduce(booleanList, (overall, current) => {
        return overall || current;
      }, false);

    }

    clearRestrictions() {
      this.uiFilter.clear();
      this.uiFilter.updateControls();
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

