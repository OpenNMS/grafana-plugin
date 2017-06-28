import {QueryCtrl} from 'app/plugins/sdk';
import './css/query-editor.css!'
import _ from 'lodash';
import {API} from '../../opennms';
import {UiFilter} from './Uifilter';

export class OpenNMSFMDatasourceQueryCtrl extends QueryCtrl {

  constructor($scope, $injector, $q,uiSegmentSrv)  {
    super($scope, $injector);
    this.$q = $q;
    this.$scope = $scope;
    this.uiSegmentSrv = uiSegmentSrv;

    // define and set up model
    this.uiFilter = new UiFilter(uiSegmentSrv);
    this.target.filter = this.target.filter || new API.Filter();

    // Only consider values which are set up correctly
    var clauses = _.filter(this.target.filter.clauses, function(clause) {
      return clause.restriction
          && clause.restriction.attribute
          && clause.restriction.comparator
          && clause.restriction.value
          && clause.operator
    });
    for (let clause of clauses) {
      this.uiFilter.addRowFromClause(clause);
    }
    this.uiFilter.addPlusButtonIfRequired();
  }

  toggleEditorMode() {
    this.target.rawQuery = !this.target.rawQuery;
  }

  onChangeInternal() {
    this.panelCtrl.refresh(); // Asks the panel to refresh data.
  }

  getSuggestions(row, segment, index) {
      var columns = row.columns;[index];
      let self = this;
      // attribute input
      if (segment.type == 'key' || segment.type == 'plus-button') {
          return this.datasource.metricFindQuery({find: "attributes"}) // TODO MVR make it attributes
                .then(function(attributes) {
                    let segments = _.map(attributes, function(attribute) {
                       var segment = self.uiSegmentSrv.newKey(attribute.name);
                       return segment;
                    });
                    return segments;
                })
                .catch(this.handleQueryError.bind(this));
      }

      // comparator input
      if (segment.type == 'operator') {
          let attributeSegment = columns[index-1];
          return this.datasource.metricFindQuery({'find': 'comparators', 'attribute': attributeSegment.value})
            .then(function(comparators) {
                return _.map(comparators, function(comparator) {
                    return self.uiSegmentSrv.newOperator(comparator);
                });
            })
            .catch(this.handleQueryError.bind(this));
      }

      // value input
      if (segment.type == 'value') {
          let attributeSegment = columns[index-2];
          let theQuery = {
              'find': 'values',
              'attribute': attributeSegment.value,
              'query': segment.value === 'select attribute value' ? '' : segment.value
          };

          return this.datasource.metricFindQuery(theQuery)
                .then(function(values) {
                    return _.map(values, function(searchResult) {
                        var segment = self.uiSegmentSrv.newKeyValue(searchResult.label);
                        return segment;
                    })
                })
                .catch(this.handleQueryError.bind(this));
      }

      // condition input
      if (segment.type === 'condition') {
          return this.datasource.metricFindQuery({find: 'operators'}).then(function(operators) {
              return _.map(operators, function(operator) {
                  return self.uiSegmentSrv.newCondition(operator.label);
              });
          }).catch(this.handleQueryError.bind(this));
      }
      return this.$q.when([]);
  }

  segmentUpdated(row, segment, segmentIndex) {
     this.uiFilter.segmentUpdated(row, segment, segmentIndex);
     this.updateTargetFilter();
  }

  removeRow(row) {
      if (this.uiFilter.removeRow(row)) {
          this.updateTargetFilter();
      }
  }

    showClearRestrictions() {
      if (this.uiFilter.getSize() == 1) {
          return this.uiFilter.table.getLastRow().getColumnCount() == 4;
      } else if (this.uiFilter.getSize() > 1) {
          return true;
      }
      return false;
    }

    clearRestrictions() {
      this.uiFilter.clear();
      this.updateTargetFilter();
    }

  updateTargetFilter() {
      this.target.filter = this.uiFilter.toFilter();
      this.panelCtrl.refresh();
  }

    getCollapsedText() {
      var collapsedText = this.uiFilter.toString();
      return collapsedText;
    }

  handleQueryError(err) {
      this.error = err.message || 'Failed to issue metric query';
      return [];
  }
}

OpenNMSFMDatasourceQueryCtrl.templateUrl = 'datasources/fault-ds/partials/query.editor.html';

