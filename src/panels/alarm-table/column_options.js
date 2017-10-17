import _ from 'lodash';
import $ from 'jquery';
import moment from 'moment';
import angular from 'angular';

import kbn from 'app/core/utils/kbn';

export class ColumnOptionsCtrl {
  /** @ngInject */
  constructor($scope, $q, uiSegmentSrv) {
    this.$q = $q;
    this.uiSegmentSrv = uiSegmentSrv;
    $scope.editor = this;
    this.activeStyleIndex = 0;
    this.panelCtrl = $scope.ctrl;
    this.panel = this.panelCtrl.panel;
    this.unitFormats = kbn.getUnitFormats();
    this.colorModes = [
      {text: 'Disabled', value: null},
      {text: 'Cell', value: 'cell'},
      {text: 'Value', value: 'value'},
      {text: 'Row', value: 'row'},
    ];
    this.columnTypes = [
      {text: 'Number', value: 'number'},
      {text: 'String', value: 'string'},
      {text: 'Date', value: 'date'},
      {text: 'Hidden', value: 'hidden'}
    ];
    this.fontSizes = ['80%', '90%', '100%', '110%', '120%', '130%', '150%', '160%', '180%', '200%', '220%', '250%'];
    this.dateFormats = [
      {text: 'DD MMM HH:mm:ss', value: 'DD MMM HH:mm:ss'},
      {text: 'YYYY-MM-DD HH:mm:ss', value: 'YYYY-MM-DD HH:mm:ss'},
      {text: 'MM/DD/YY h:mm:ss a', value: 'MM/DD/YY h:mm:ss a'},
      {text: 'MMMM D, YYYY LT', value: 'MMMM D, YYYY LT'},
    ];

    this.getColumnNames = () => {
      if (!this.panelCtrl.table) {
        return [];
      }
      return _.map(this.panelCtrl.table.columns, function (col) {
        return col.text;
      });
    };
  }

  render() {
    this.panelCtrl.render();
  }

  setUnitFormat(column, subItem) {
    column.unit = subItem.value;
    this.panelCtrl.render();
  }

  addColumnStyle() {
    let newStyleRule = {
      unit: 'short',
      type: 'number',
      alias: '',
      decimals: 2,
      colors: ["rgba(245, 54, 54, 0.9)", "rgba(237, 129, 40, 0.89)", "rgba(50, 172, 45, 0.97)"],
      colorMode: null,
      pattern: '',
      dateFormat: 'YYYY-MM-DD HH:mm:ss',
      thresholds: [],
      width: "",
      clip: false
    };

    let styles = this.panel.styles;
    let stylesCount = styles.length;
    let indexToInsert = stylesCount;

    // check if last is a catch all rule, then add it before that one
    if (stylesCount > 0) {
      let last = styles[stylesCount - 1];
      if (last.pattern === '/.*/') {
        indexToInsert = stylesCount - 1;
      }
    }

    styles.splice(indexToInsert, 0, newStyleRule);
    this.activeStyleIndex = indexToInsert;
  }

  removeColumnStyle(style) {
    this.panel.styles = _.without(this.panel.styles, style);
  }

  invertColorOrder(index) {
    let ref = this.panel.styles[index].colors;
    let copy = ref[0];
    ref[0] = ref[2];
    ref[2] = copy;
    this.panelCtrl.render();
  }
}

/** @ngInject */
export function columnOptionsTab($q, uiSegmentSrv) {
  'use strict';
  return {
    restrict: 'E',
    scope: true,
    templateUrl: 'public/plugins/opennms-helm-app/panels/alarm-table/column_options.html',
    controller: ColumnOptionsCtrl,
  };
}
