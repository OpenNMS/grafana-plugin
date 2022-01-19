import _ from 'lodash';

import { transformers, TableTransform } from './transformers';

export const fontSizes = ['80%', '90%', '100%', '110%', '120%', '130%', '150%', '160%', '180%', '200%', '220%', '250%'];

export class TablePanelEditorCtrl {
  addColumnSegment: any;
  panel: any;
  panelCtrl: any;
  transformers: { [key: string]: TableTransform };

  canSetColumns = false;
  columnsHelpMessage = '';
  fontSizes: string[];
  themes: { [key: string]: string };
  srcIndex?: number;
  destIndex?: number;

  /** @ngInject */
  constructor(public $scope: any, public uiSegmentSrv: any) {
    $scope.editor = this;
    this.panelCtrl = $scope.ctrl;
    this.panel = this.panelCtrl.panel;
    this.transformers = transformers;
    this.fontSizes = [...fontSizes];

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
      //console.debug('adding listener: ' + e);
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
      [].forEach.call(cols, (col: Element) => {
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
          console.info(`picking up "${this.panel.columns[this.srcIndex].text}"`);
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
            //console.debug(`entering ${this.panel.columns[columnIndex].text}`);
            this.removeClasses('over');
            target.classList.add('over');
            this.destIndex = columnIndex;
          }
        }
        break;
      case 'dragleave':
        if (target && evt.screenX !== 0 && evt.screenY !== 0) {
          //const columnIndex = parseInt(target.id.replace(/^column-/, ''), 10);
          //console.debug(`leaving ${this.panel.columns[columnIndex].text}`);
          this.destIndex = undefined;
          this.removeClasses('over');
        }
        break;
      case 'drop':
        if (evt.stopPropagation) {
          evt.stopPropagation();
        }
        if (this.srcIndex !== undefined && this.destIndex !== undefined) {
          this.$scope.$apply(() => {
            this.panel.columns.splice(this.destIndex, 0, this.panel.columns.splice(this.srcIndex, 1)[0]);
            this.panelCtrl.render();
          });
          console.info(`dropped "${this.panel.columns[this.srcIndex].text}" onto "${this.panel.columns[this.destIndex].text}"`);
        } else {
          const targetIndex = (this.srcIndex === undefined) ? 'source' : 'destination';
          console.warn(`drop event received but ${targetIndex} was unset.`);
        }
        this.removeClasses('over', 'picked-up');
        return false;
      default:
        console.error(`unhandled event type: ${type}`);
    }
    return true;
  }

  getColumnOptions() {
    if (!this.panelCtrl.dataRaw) {
      return Promise.resolve([]);
    }
    let columns = this.transformers[this.panel.transform].getColumns(this.panelCtrl.dataRaw);
    // Filter out columns that have already been selected
    columns = columns.filter(a => this.panel.columns.indexOf(a) < 0);

    const segments = _.map(columns, (c) => this.uiSegmentSrv.newSegment({value: c.text}));
    return Promise.resolve(segments);
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
