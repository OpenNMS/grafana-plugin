import _ from 'lodash';
import { API } from 'opennms';

import { QueryCtrl } from 'grafana/app/plugins/sdk';

import { Mapping } from './Mapping';
import { UI } from './UI';
import { entityTypes, getEntity } from './datasource';
import './query-directive';

import './css/query-editor.css';

export class OpenNMSEntityDatasourceQueryCtrl extends QueryCtrl {
  static templateUrl = 'public/plugins/opennms-helm-app/datasources/entity-ds/partials/query.editor.html';

  datasource: any;
  entity: any;
  entityTypes: any;
  error: any;
  featuredAttributes: boolean;
  filterMapping: any;
  panelCtrl: any;
  target: any;
  uiFilter: any;

  /** @ngInject */
  constructor($scope: any, $injector: any, public $q: any, public uiSegmentSrv) {
    super($scope, $injector);

    this.featuredAttributes = true; // limits the selection to the featured attributes
    this.entityTypes = entityTypes;

    if (!this.target) {
      this.target = {};
    }

    // delete this.target.entityType;
    if (!this.target.entityType) {
      this.target.entityType = this.entityTypes[0];
    }

    if (this.target.limit === undefined) {
      this.target.limit = 0;
    }

    if (this.target.orderBy === undefined) {
      this.target.orderBy = [];
    }

    // The target filter may be de-serialized from persistence.
    // In order to re-initialize it properly, the filter is cloned.
    if (this.target.filter) {
      this.target.filter = API.Filter.fromJson(this.target.filter);
    } else {
      this.target.filter = new API.Filter();
    }

    // initialize the UI filter
    this._getUiFilter().updateControls();
  }

  _getEntity() {
    if (this.target?.entityType?.id !== this.entity?.type) {
      this.entity = getEntity(this.target?.entityType?.id, undefined, this.datasource);
    }
    return this.entity;
  }

  _getFilterMapping() {
    const entity = this._getEntity();
    if (this.filterMapping?.entity?.type !== entity?.type) {
      this.filterMapping = new Mapping.FilterMapping(this.uiSegmentSrv, entity);
    }
    return this.filterMapping;
  }

  _getUiFilter() {
    const filterMapping = this._getFilterMapping();
    if (!this.uiFilter) {
      this.uiFilter = filterMapping.getUiFilter(this.target?.filter);
    } else if (this.uiFilter?.entity?.type !== filterMapping.entity?.type) {
      this.uiFilter.entity = filterMapping.entity;
    }
    return this.uiFilter;
  }

  toggleEditorMode() {
    this.target.rawQuery = !this.target?.rawQuery;
  }

  onChangeEntityType() {
    const filterMapping = this._getFilterMapping();
    this.uiFilter.entity = filterMapping?.entity;
    this.clearRestrictions();
  }

  onChangeInternal() {
    this.panelCtrl.refresh(); // Asks the panel to refresh data.
  }

  showClearRestrictions(query = this._getUiFilter().query) {
    const self = this;
    const booleanList = _.map(query.clauses, (clause) => {
      if (clause.restriction instanceof UI.Query) {
        return self.showClearRestrictions(clause.restriction);
      }
      return new UI.Controls.RemoveControl().filter(query, clause);
    });

    return _.reduce(
      booleanList,
      (overall, current) => {
        return overall || current;
      },
      false
    );
  }

  clearRestrictions() {
    const uiFilter = this._getUiFilter();
    uiFilter.clear();
    uiFilter.updateControls();
    this.updateTargetFilter();
  }

  updateTargetFilter() {
    const uiFilter = this._getUiFilter();
    this.target.filter = this.filterMapping.getApiFilter(uiFilter);
    this.panelCtrl.refresh();
  }

  getCollapsedText() {
    var collapsedText = this._getUiFilter().getQueryString();
    return collapsedText;
  }

  handleQueryError(err) {
    this.error = err.message || 'Failed to issue metric query';
    return [];
  }
}
