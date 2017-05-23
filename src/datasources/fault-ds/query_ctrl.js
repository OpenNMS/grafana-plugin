import {QueryCtrl} from 'app/plugins/sdk';
import './css/query-editor.css!'
import _ from 'lodash';
import {OpenNMSFMDatasourceSearchService} from './search_main_ctrl';

export class OpenNMSFMDatasourceQueryCtrl extends QueryCtrl {

  constructor($rootScope, $scope, $injector, $q, $modal)  {
    super($scope, $injector);
    this.$q = $q;
    this.$modal = $modal;
    this.$scope = $scope;
    this.$rootScope  = $rootScope;
    this.comparators = {};
    this.fields = [];
    this.searchService = new OpenNMSFMDatasourceSearchService(this.datasource, $rootScope, $q, $modal);
    this.target.restrictions = [];
    this.getRestrictions();
    this.addRestriction(); // ensure that there is at least one row
      // TODO MVR reloading an persisted state probably won't work
  }

  toggleEditorMode() {
    this.target.rawQuery = !this.target.rawQuery;
  }

  onChangeInternal() {
    this.panelCtrl.refresh(); // Asks the panel to refresh data.
  }

  getRestrictions() {
    let that = this;
    this.datasource.metricFindQuery({find: "fields"})
        .then(function(result) {
            that.fields = result;
        })
        .catch(this.handleQueryError.bind(this));
  }

  updateComparators(restriction) {
    let that = this;
    let theField = restriction.field;
    this.datasource.metricFindQuery({'find': 'comparators', 'field': theField})
        .then(function(result) {
            that.comparators[theField.type] = result;
        })
        .catch(this.handleQueryError.bind(this));
  }

  getCollapsedText() {
    let restrictions = this.target.restrictions.filter(function(field) {
      return field.name != 'select field' && field.comparator != 'select comparator' && field.value != '';
    });


    if (restrictions.length == 0) {
      return "empty query"
    }

    if (restrictions.length == 1) {
      return this.convert(restrictions[0]);
    }

    let that = this;
    let collapsedText = restrictions.map(function(restriction) {
      return "(" + that.convert(restriction) + ")"
    }).join(" AND ");

    return collapsedText;
  }

  convert(restriction) {
    let theValue = restriction.field.type === 'number' ? restriction.value : "'" + restriction.value + "'";
    return restriction.field.name + " " + restriction.comparator + " " + theValue;
  }

  isSingleRestriction() {
    return this.target.restrictions.length == 1;
  }

  isLast(restriction) {
      let index = this.target.restrictions.indexOf(restriction);
      if (index >= 0) {
        return index == this.target.restrictions.length - 1;
      }
      return false;
  }

  addRestriction() {
    let index = this.target.restrictions.length;
    let newRestriction = {
        field: {
            name: 'select field',
            description: 'Please select a field',
            type: 'undefined',
        },
        comparator: 'select comparator',
        value: ''

    };
    this.target.restrictions.splice(index, 0, newRestriction);

  }

  removeRestriction(restriction) {
    let index = this.target.restrictions.indexOf(restriction);
    if (index >= 0) {
      this.target.restrictions.splice(index, 1);
    }
  }

  isShowSelectionWindow(restriction) {
    let types = ['user', 'node', 'category', 'location'];
    let field = restriction.field;
    let index = types.indexOf(field.type);
    let show = index >= 0;
    // console.log("Show selection window for field: " + field.name + "/" + field.type + " (index=" + index + ") : " + show);
    return show;
  };

  showSelectionWindow(restriction) {
    let self = this;
    if (restriction.field.type == 'node') {
        self.searchService.search({find: 'nodes'}, function (node) {
            restriction.value = node.id;
        });
    }
  }

  handleQueryError(err) {
      this.error = err.message || 'Failed to issue metric query';
      return [];
  }
}

OpenNMSFMDatasourceQueryCtrl.templateUrl = 'datasources/fault-ds/partials/query.editor.html';

