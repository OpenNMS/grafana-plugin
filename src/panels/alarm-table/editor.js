import _ from 'lodash';

import {transformers} from './transformers';

export class TablePanelEditorCtrl {
  /** @ngInject */
  constructor($scope, $q, uiSegmentSrv) {
    this.$q = $q;
    this.$scope = $scope;
    this.uiSegmentSrv = uiSegmentSrv;
    $scope.editor = this;
    this.panelCtrl = $scope.ctrl;
    this.panel = this.panelCtrl.panel;
    this.transformers = transformers;
    this.fontSizes = ['80%', '90%', '100%', '110%', '120%', '130%', '150%', '160%', '180%', '200%', '220%', '250%'];

    this.themes = {
      helm: 'Helm Default',
      opennms: 'OpenNMS',
      omi: 'Oh My!',
      nnmi: 'No, Never Mind (i)',
      netcool: "That's Cool",
    };

    if (!this.themes[this.panel.theme]) {
      this.panel.theme = 'helm';
    }

    this.srcIndex = undefined;
    this.destIndex = undefined;

    this.addColumnSegment = uiSegmentSrv.newPlusButton();
    this.updateTransformHints();

    const editor = document.querySelectorAll('.editor-row')[0];
    for (const e of [ 'dragstart', 'dragover', 'dragleave', 'drop']) {
      //console.log('adding listener: ' + e);
      editor.addEventListener(e, (evt) => { this.handleEvent(e, evt); }, false);
    }
  }

  updateTransformHints() {
    // BMR: alarm table can always choose which columns to include, and in what order
    this.canSetColumns = true;
    this.columnsHelpMessage = '';

    switch (this.panel.transform) {
      case 'timeseries_aggregations': {
        this.canSetColumns = true;
        break;
      }
      case 'json': {
        this.canSetColumns = true;
        break;
      }
      case 'table': {
        this.columnsHelpMessage = 'Columns and their order are determined by the data query';
      }
    }
  }

  removeClasses(...classes) {
    for (const c of classes) {
      const cols = document.querySelectorAll('.' + c);
      [].forEach.call(cols, (col) => {
        col.classList.remove(c);
      });
    }
  }

  getTarget(evt) {
    if (evt.srcElement && evt.srcElement.offsetParent) {
      let target = evt.srcElement.offsetParent;
      if (target && target.id && target.classList && target.classList.contains('column-reorder')) {
        return target;
      }
    }
    // dragleave is only fired for the label and not the parent container
    if (evt.target && evt.target.parent && evt.target.parent.classList && evt.target.parent.classList.contains('column-reorder')) {
      return evt.target.parent;
    }
  }

  handleEvent(type, evt) {
    const target = this.getTarget(evt);
    const id = evt.srcElement.id;

    switch(type) {
      case 'dragstart':
        evt.srcElement.classList.add('picked-up');
        evt.dataTransfer.effectAllowed = 'move';
        // Internet Explorer doesn't support "text/html":
        // https://stackoverflow.com/a/28740710
        try {
          evt.dataTransfer.setData('text/html', evt.srcElement.innerHTML);
        } catch (error) {
          evt.dataTransfer.setData('text', evt.srcElement.innerHTML);
        }
        if (id) {
          this.srcIndex = parseInt(id.replace(/^column-/, ''), 10);
          console.log('picking up "' + this.panel.columns[this.srcIndex].text + '"');
        }
        break;
      case 'dragover':
        if (evt.preventDefault) {
          evt.preventDefault();
        }
        evt.dataTransfer.dropEffect = 'move';
        if (target && target.id && target.classList && target.classList.contains('column-reorder')) {
          const columnIndex = parseInt(target.id.replace(/^column-/, ''), 10);
          if (!target.classList.contains('over')) {
            //console.log('entering ' + this.panel.columns[columnIndex].text);
            this.removeClasses('over');
            target.classList.add('over');
            this.destIndex = columnIndex;
          }
        }
        break;
      case 'dragleave':
        if (target && evt.screenX !== 0 && evt.screenY !== 0) {
          //const columnIndex = parseInt(target.id.replace(/^column-/, ''), 10);
          //console.log('leaving ' + this.panel.columns[columnIndex].text);
          this.destIndex = undefined;
          this.removeClasses('over');
        }
        break;
      case 'drop':
        if (eval.stopPropagation) {
          evt.stopPropagation();
        }
        if (this.srcIndex !== undefined && this.destIndex !== undefined) {
          this.$scope.$apply(() => {
            this.panel.columns.splice(this.destIndex, 0, this.panel.columns.splice(this.srcIndex, 1)[0]);
            this.panelCtrl.render();
          });
          console.log('dropped "' + this.panel.columns[this.srcIndex].text + '" onto "' + this.panel.columns[this.destIndex].text + '"');
        } else {
          const targetIndex = (this.srcIndex == undefined) ? 'source' : 'destination';
          console.log(`WARNING: drop event received but ${targetIndex} was unset.`);
        }
        this.removeClasses('over', 'picked-up');
        return false;
      default:
        console.log('WARNING: unhandled event type: ' + type);
    }
  }

  getColumnOptions() {
    if (!this.panelCtrl.dataRaw) {
      return this.$q.when([]);
    }
    let columns = this.transformers[this.panel.transform].getColumns(this.panelCtrl.dataRaw);
    // Filter out columns that have already been selected
    columns = columns.filter(a => this.panel.columns.indexOf(a) < 0);

    const segments = _.map(columns, (c) => this.uiSegmentSrv.newSegment({value: c.text}));
    return this.$q.when(segments);
  }

  addColumn() {
    const columns = transformers[this.panel.transform].getColumns(this.panelCtrl.dataRaw);
    const column = _.find(columns, {text: this.addColumnSegment.value});

    if (column) {
      this.panel.columns.push(column);
      this.render();
    }

    const plusButton = this.uiSegmentSrv.newPlusButton();
    this.addColumnSegment.html = plusButton.html;
    this.addColumnSegment.value = plusButton.value;
  }

  transformChanged() {
    this.panel.columns = [];
    if (this.panel.transform === 'timeseries_aggregations') {
      this.panel.columns.push({text: 'Avg', value: 'avg'});
    }

    this.updateTransformHints();
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
export function tablePanelEditor($q, uiSegmentSrv) { // eslint-disable-line no-unused-vars
  'use strict';
  return {
    restrict: 'E',
    scope: true,
    templateUrl: 'public/plugins/opennms-helm-app/panels/alarm-table/editor.html',
    controller: TablePanelEditorCtrl,
  };
}
