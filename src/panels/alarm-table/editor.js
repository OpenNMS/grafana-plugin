import _ from 'lodash';
import $ from 'jquery';
import moment from 'moment';
import angular from 'angular';

import {transformers} from './transformers';
import kbn from 'app/core/utils/kbn';

export class TablePanelEditorCtrl {
  /** @ngInject */
  constructor($scope, $q, uiSegmentSrv) {
    this.$q = $q;
    this.uiSegmentSrv = uiSegmentSrv;
    $scope.editor = this;
    this.panelCtrl = $scope.ctrl;
    this.panel = this.panelCtrl.panel;
    this.transformers = transformers;
    this.fontSizes = ['80%', '90%', '100%', '110%', '120%', '130%', '150%', '160%', '180%', '200%', '220%', '250%'];

    this.addColumnSegment = uiSegmentSrv.newPlusButton();
  }

  getColumnOptions() {
    if (!this.panelCtrl.dataRaw) {
      return this.$q.when([]);
    }
    let columns = this.transformers[this.panel.transform].getColumns(this.panelCtrl.dataRaw);
    // Filter out columns that have already been selected
    let self = this;
    columns = columns.filter(function(a){return self.panel.columns.indexOf(a) < 0});
    let segments = _.map(columns, (c) => this.uiSegmentSrv.newSegment({value: c.text}));
    return this.$q.when(segments);
  }

  addColumn() {
    let columns = transformers[this.panel.transform].getColumns(this.panelCtrl.dataRaw);
    let column = _.find(columns, {text: this.addColumnSegment.value});

    if (column) {
      this.panel.columns.push(column);
      this.render();
    }

    let plusButton = this.uiSegmentSrv.newPlusButton();
    this.addColumnSegment.html = plusButton.html;
    this.addColumnSegment.value = plusButton.value;
  }

  transformChanged() {
    this.panel.columns = [];
    if (this.panel.transform === 'timeseries_aggregations') {
      this.panel.columns.push({text: 'Avg', value: 'avg'});
    }

    this.render();
  }

  render() {
    this.panelCtrl.render();
  }

  removeColumn(column) {
    this.panel.columns = _.without(this.panel.columns, column);
    this.panelCtrl.render();
  }
}

/** @ngInject */
export function tablePanelEditor($q, uiSegmentSrv) {
  'use strict';
  return {
    restrict: 'E',
    scope: true,
    templateUrl: '/public/plugins/opennms-helm-app/panels/alarm-table/editor.html',
    controller: TablePanelEditorCtrl,
  };
}
