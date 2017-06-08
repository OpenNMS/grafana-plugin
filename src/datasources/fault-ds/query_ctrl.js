import {QueryCtrl} from 'app/plugins/sdk';
import './css/query-editor.css!'
import _ from 'lodash';
import {AlarmQuery} from './alarm_query';

export class OpenNMSFMDatasourceQueryCtrl extends QueryCtrl {

  constructor($scope, $injector, $q,uiSegmentSrv)  {
    super($scope, $injector);
    this.$q = $q;
    this.$scope = $scope;
    this.uiSegmentSrv = uiSegmentSrv;

    // define model
    this.target.restrictions = this.target.restrictions || [];
    this.restrictionGroupSegments = [];

    for (let restriction of this.target.restrictions) {
        let restrictionSegments = [];
        restrictionSegments.push(uiSegmentSrv.newKey(restriction.attribute));
        restrictionSegments.push(uiSegmentSrv.newOperator(restriction.comparator));
        restrictionSegments.push(uiSegmentSrv.newKeyValue(restriction.value));

        this.restrictionGroupSegments.push(restrictionSegments);
    }

    this.addPlusButtonIfRequired();
  }

  toggleEditorMode() {
    this.target.rawQuery = !this.target.rawQuery;
  }

  onChangeInternal() {
    this.panelCtrl.refresh(); // Asks the panel to refresh data.
  }

  addPlusButtonIfRequired() {
    var groupCount = this.restrictionGroupSegments.length;
    if (groupCount == 0) {
        this.restrictionGroupSegments.push([]);
    }
    let groupIndex = Math.max(groupCount-1, 0);
    let group = this.restrictionGroupSegments[groupIndex];
    let segmentIndex = Math.max(group.length - 1, 0);
    let lastSegment = group[segmentIndex];
    if (!lastSegment || lastSegment.type !== 'plus-button') {
        group.push(this.uiSegmentSrv.newPlusButton());
    }
  }

  getSuggestions(group, segment, index) {
      var restrictionSegments = group;

      let that = this;
      // attribute input
      if (segment.type == 'key' || segment.type == 'plus-button') {
          return this.datasource.metricFindQuery({find: "attributes"}) // TODO MVR make it attributes
                .then(function(attributes) {
                    let segments = _.map(attributes, function(attribute) {
                       var segment = that.uiSegmentSrv.newKey(attribute.name);
                       return segment;
                    });
                    return segments;
                })
                .catch(this.handleQueryError.bind(this));
      }

      // comparator input
      if (segment.type == 'operator') {
          let attributeSegment = restrictionSegments[index-1];
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
          let attributeSegment = restrictionSegments[index-2];
          let theQuery = {
              'find': 'values',
              'attribute': attributeSegment.value,
              'query': segment.value === 'select attribute value' ? '' : segment.value
          };

          return this.datasource.metricFindQuery(theQuery)
                .then(function(values) {
                    return _.map(values, function(searchResult) {
                        var segment = that.uiSegmentSrv.newKeyValue(searchResult.label);
                        return segment;
                    })
                })
                .catch(this.handleQueryError.bind(this));
      }
      return this.$q.when([]);
  }

  segmentUpdated(group, segment, index) {
      var restrictionSegments = group;

      if (segment.type === 'plus-button') {
          // make the plus button an actual attribute input
          segment.type = 'key';
          segment.cssClass = 'query-segment-key';

          if (index > 0) {
              // remove plus button from current group
              group.splice(group.indexOf(segment), 1);

              // create new group
              this.restrictionGroupSegments.push([]);
              restrictionSegments = this.restrictionGroupSegments[this.restrictionGroupSegments.length - 1];

              // add key (was plus button)
              restrictionSegments.push(segment);
          }

          // Add comparator and value
          restrictionSegments.push(this.uiSegmentSrv.newOperator('='));
          restrictionSegments.push(this.uiSegmentSrv.newFake('select attribute value', 'value', 'query-segment-value'));

          // reset index
          index = 0;
      }
      if (segment.type == 'value') {
          segment.fake = false;
      }

      this.updateTargetRestrictions();

      // Ensure that we always have a plus button
      if ((index + 1) === restrictionSegments.length) {
          restrictionSegments.push(this.uiSegmentSrv.newPlusButton());
      }
  }

  removeGroup(groupSegment) {
      var index = this.restrictionGroupSegments.indexOf(groupSegment);
      if (index >= 0) {
        this.restrictionGroupSegments.splice(index, 1);
        this.addPlusButtonIfRequired();
        this.updateTargetRestrictions();
    }
  }

    removeRestriction(group) {
        var index = this.restrictionGroupSegments.indexOf(group);
        if (index > -1) {
            this.restrictionGroupSegments.splice(index, 1);
            this.addPlusButtonIfRequired();
            this.updateTargetRestrictions();
        }
    }

    showClearRestrictions() {
      if (this.restrictionGroupSegments.length == 1) {
          return this.restrictionGroupSegments[0].length == 4;
      } else if (this.restrictionGroupSegments.length > 1) {
          return true;
      }
      return false;
    }

    clearRestrictions() {
      this.restrictionGroupSegments = [];
      this.addPlusButtonIfRequired();
      this.updateTargetRestrictions();
    }

  updateTargetRestrictions() {
      var restrictions = [];
      var restrictionGroupSegments = this.restrictionGroupSegments;
      _.each(restrictionGroupSegments, function(eachGroup, groupIndex) {
          var restrictionSegments = _.filter(eachGroup, function(segment) {
              return segment.type !== 'plus-button' && (segment.fake === undefined || segment.fake === false)
          });
          if (restrictionSegments.length > 0 && restrictionSegments.length % 3 == 0) {
              _.each(restrictionSegments, (segment, segmentIndex) => {
                  if (segment.type === 'key') {
                      restrictions.push({});
                      restrictions[groupIndex].attribute = segment.value;
                  } else if (segment.type === 'operator') {
                      restrictions[groupIndex].comparator = segment.value;
                  } else if (segment.type === 'value') {
                      restrictions[groupIndex].value = segment.value;
                  }
              });
          }
      });

      this.target.restrictions = restrictions;
      this.panelCtrl.refresh()

  }

    getCollapsedText() {
        var query = new AlarmQuery(this.target.restrictions).render();
        return query;
    }

  handleQueryError(err) {
      this.error = err.message || 'Failed to issue metric query';
      return [];
  }
}

OpenNMSFMDatasourceQueryCtrl.templateUrl = 'datasources/fault-ds/partials/query.editor.html';

