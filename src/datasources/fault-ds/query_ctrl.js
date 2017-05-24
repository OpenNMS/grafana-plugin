import {QueryCtrl} from 'app/plugins/sdk';
import './css/query-editor.css!'
import _ from 'lodash';

export class OpenNMSFMDatasourceQueryCtrl extends QueryCtrl {

  constructor($rootScope, $scope, $injector, $q, $modal, uiSegmentSrv)  {
    super($scope, $injector);
    this.$q = $q;
    this.$modal = $modal;
    this.$scope = $scope;
    this.$rootScope  = $rootScope;
    this.uiSegmentSrv = uiSegmentSrv;

    // define model
    this.target.restrictions = this.target.restrictions || [];
    this.restrictionSegments = [];
    for (let restriction of this.target.restrictions) {
      // if (!restriction.comparator) {
      //     if (/^\/.*\/$/.test(restriction.value)) {
      //         restriction.operator = "=~"; // TODO MVR this is not supported by our datasource
      //     } else {
      //         restriction.operator = '=';
      //     }
      // }

      // if (restriction.condition) {
      //     this.tagSegments.push(uiSegmentSrv.newCondition(restriction.condition));
      // }

      this.restrictionSegments.push(uiSegmentSrv.newKey(restriction.attribute));
      this.restrictionSegments.push(uiSegmentSrv.newOperator(restriction.comparator));
      this.restrictionSegments.push(uiSegmentSrv.newKeyValue(restriction.value));
    }

    this.addPlusButtonIfRequired();

      // TODO MVR reloading an persisted state probably won't work
  }

  toggleEditorMode() {
    this.target.rawQuery = !this.target.rawQuery;
  }

  onChangeInternal() {
    this.panelCtrl.refresh(); // Asks the panel to refresh data.
  }

  addPlusButtonIfRequired() {
    let count = this.restrictionSegments.length;
    let lastSegment = this.restrictionSegments[Math.max(count-1, 0)];

    if (!lastSegment || lastSegment.type !== 'plus-button') {
      this.restrictionSegments.push(this.uiSegmentSrv.newPlusButton());
    }
  }

  getSuggestions(segment, index) {
      let that = this;
      // attribute input
      if (segment.type == 'key' || segment.type == 'plus-button') {
          return this.datasource.metricFindQuery({find: "attributes"}) // TODO MVR make it attributes
                .then(function(attributes) {
                    let segments = _.map(attributes, function(attribute) {
                       return that.uiSegmentSrv.newKey(attribute.name);
                    });
                    return segments;
                })
                .catch(this.handleQueryError.bind(this));
      }

      // comparator input
      if (segment.type == 'operator') {
          let attributeSegment = this.restrictionSegments[index-1];
          return this.datasource.metricFindQuery({'find': 'comparators', 'attribute': attributeSegment.value})
            .then(function(comparators) {
                return _.map(comparators, function(comparator) {
                    return that.uiSegmentSrv.newOperator(comparator);
                });
            })
            .catch(this.handleQueryError.bind(this));
      }

      // value input
      if (segment.type == 'value') {
          let attributeSegment = this.restrictionSegments[index-2];
          let theQuery = {
              'find': 'values',
              'attribute': attributeSegment.value,
              'query': segment.value === 'select attribute value' ? '' : segment.value
          };

          return this.datasource.metricFindQuery(theQuery)
                .then(function(values) {
                    return _.map(values, function(searchResult) {
                        return that.uiSegmentSrv.newKeyValue(searchResult.label);
                    })
                })
                .catch(this.handleQueryError.bind(this));
      }
      return this.$q.when([]);
  }

  segmentUpdated(segment, index) {
      if (segment.type === 'plus-button') {
          // make the plus button an actual attribute input
          segment.type = 'key';
          segment.cssClass = 'query-segment-key';

          // Add comparator and value
          this.restrictionSegments.push(this.uiSegmentSrv.newOperator('='));
          this.restrictionSegments.push(this.uiSegmentSrv.newFake('select attribute value', 'value', 'query-segment-value'));
      }
      if (segment.type == 'value') {
          segment.fake = false;
      }

      this.updateTargetRestrictions();

      // Ensure that we always have a plus button
      if ((index + 1) === this.restrictionSegments.length) {
          this.restrictionSegments.push(this.uiSegmentSrv.newPlusButton());
      }
  }

  updateTargetRestrictions() {
      var restrictionSegments = _.filter(this.restrictionSegments, function(segment) {
          return segment.type !== 'plus-button' && (segment.fake === undefined || segment.fake === false)
      });
      var restrictions = [];
      if (restrictionSegments.length > 0 && restrictionSegments.length % 3 == 0) {
          _.each(restrictionSegments, (segment, index) => {
              var restrictionIndex = Math.floor(index / 3);
              if (segment.type === 'key') {
                  restrictions.push({});
                  restrictions[restrictionIndex].attribute = segment.value;
              } else if (segment.type === 'value') {
                  restrictions[restrictionIndex].value = segment.value;
              } else if (segment.type === 'operator') {
                  restrictions[restrictionIndex].comparator = segment.value;
              }
          });
      }
      this.target.restrictions = restrictions;
      this.panelCtrl.refresh()

  }

  getCollapsedText() {
    if (this.target.restrictions.length == 0) {
      return "query all alarms"
    }

    if (this.target.restrictions.length == 1) {
      return this.convert(this.target.restrictions[0]);
    }

    let that = this;
    let collapsedText = this.target.restrictions.map(function(restriction) {
      return "(" + that.convert(restriction) + ")"
    }).join(" AND ");

    return collapsedText;
  }

  convert(restriction) {
    // var attribute = this.alarmClient.findAttribute(restriction.attribute);
    // let theValue = attribute.type === 'number' ? restriction.value : "'" + restriction.value + "'";
    return restriction.attribute + " " + restriction.comparator + " " + restriction.value;
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

  handleQueryError(err) {
      this.error = err.message || 'Failed to issue metric query';
      return [];
  }
}

OpenNMSFMDatasourceQueryCtrl.templateUrl = 'datasources/fault-ds/partials/query.editor.html';

