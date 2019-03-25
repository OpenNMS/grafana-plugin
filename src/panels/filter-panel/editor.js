import { FilterColumn } from '../../lib/filter_column';

import {entityTypes} from '../../datasources/entity-ds/datasource';

export class FilterPanelEditorCtrl {
  /** @ngInject */
  constructor($scope, $q, uiSegmentSrv, datasourceSrv) {
    this.$q = $q;
    this.$scope = $scope;
    this.uiSegmentSrv = uiSegmentSrv;
    this.datasourceSrv = datasourceSrv;
    $scope.editor = this;
    this.panelCtrl = $scope.ctrl;
    this.panel = this.panelCtrl.panel;
    $scope.panel = this.panel;

    this.entityTypes = entityTypes;
    
    if (!$scope.current) {
      $scope.current = {};
    }
    if (!$scope.current.datasourceType) {
      $scope.current.datasourceType = undefined;
    }
    if (!$scope.current.entityType) {
      $scope.current.entityType = entityTypes[0];
    }

    this.srcIndex = undefined;
    this.destIndex = undefined;

    this.addColumnSegment = uiSegmentSrv.newPlusButton();
    let editor = document.querySelectorAll('.editor-row')[0];
    for (const e of [ 'dragstart', 'dragover', 'dragleave', 'drop']) {
      editor.addEventListener(e, (evt) => { this.handleEvent(e, evt); }, false);
    }

    $scope.reset = this.reset;
    this.reset();
  }

  async updateCurrent() {
    await this.$scope.$evalAsync(() => {});

    const current = this.$scope.current;

    const dsName = current.datasource && current.datasource.name ? current.datasource.name : undefined;
    await this.setDatasource(dsName);
    this.setEntityType(current.entityType);
    this.render();
  }

  async setDatasource(dsName) {
    console.debug('Setting datasource to: ' + dsName);
    try {
      const ds = await this.getDatasource(dsName);
      this.$scope.current.datasourceType = ds.type;
    } catch(err) {
      console.warn('Failed to get datasource ' + dsName, err);
      this.$scope.current.datasourceType = undefined;
    }
  }

  setEntityType(type) {
    const t = type ? type : this.entityTypes[0];
    this.$scope.current.entityType = t;
    console.debug('Setting entity type to:', t);
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

  async getColumnOptions() {
    const self = this;
    const $scope = self.$scope;
    const dsName = $scope.datasource;

    const ds = await self.getDatasource(dsName);

    await $scope.$evalAsync(() => {});

    const entityType = ds.type === 'opennms-helm-entity-datasource' ? $scope.current.entityType : undefined;

    const opts = {
      queryType: 'attributes'
    };
    if (entityType) {
      opts.entityType = entityType.id;
    }

    const res = await ds.metricFindQuery(entityType ? entityType.queryFunction + '()' : null, opts);
    console.log('getColumnOptions: metricFindQuery result:', res);

    // filter out columns that have already been selected
    const filtered = res.filter(a => self.panel.columns.indexOf(a) < 0);

    const segments = filtered.map(c => this.uiSegmentSrv.newSegment({
      // datasource: ds.name,
      value: c.name
    }));

    return segments;
  }

  async addColumn() {
    const self = this;
    const $scope = self.$scope;

    const label = self.addColumnSegment.value;

    const ds = await self.getDatasource($scope.current.datasource);

    const entityType = ds.type === 'opennms-helm-entity-datasource' ? $scope.current.entityType : undefined;

    const opts = {
      queryType: 'attributes'
    };
    if (entityType) {
      opts.entityType = entityType.id;
    }

    const res = await ds.metricFindQuery(entityType ? entityType.queryFunction + '()' : null, opts);
    console.log('addColumn: metricFindQuery result:', res);

    const match = res.filter(col => col.name === label)[0];
    if (match) {
      const label = match.name;
      const column = new FilterColumn(label, ds.name, match.id, 'multi', entityType);
      console.debug('adding column:', column);
      self.panel.columns.push(column);
    }

    const plusButton = self.uiSegmentSrv.newPlusButton();
    self.addColumnSegment.html = plusButton.html;
    self.addColumnSegment.value = plusButton.value;
    self.render();
  }

  render() {
    this.panelCtrl.render();
  }

  async getDatasource(ds) {
    const dsName = ds && ds.name ? ds.name : ds;
    const d = await this.datasourceSrv.get(dsName);
    console.debug('getDatasource: ' + (dsName ? dsName : 'default') + '=', d);
    return d;
  }

  getMetricSource(ds) {
    if (!ds || !ds.name) {
      return null;
    }
    return {
      name: ds.name,
      value: ds.name,
      sort: ds.name,
      meta: ds
    };
  }

  getDatasources() {
    const sources = this.datasourceSrv.getMetricSources();
    return sources.filter(ds => {
      return !ds.meta.mixed && ds.value !== null;
    });
  }

  async reset() {
    this.$scope.datasources = this.getDatasources();

    const current = this.$scope.current;
    if (this.panel.datasource) {
      const ds = await this.getDatasource(this.panel.datasource);
      current.datasource = this.getMetricSource(ds);
    }  else {
      const ds = await this.getDatasource();
      current.datasource = this.$scope.datasources.filter((existing) => {
        return ds.name === existing.name;
      })[0];
    }

    current.entityType =  undefined;
    await this.updateCurrent();
  }

  async removeColumn(column, index) {
    console.debug('removing column:', column);
    this.panel.columns.splice(index, 1);
    await this.reset();
  }
}

/** @ngInject */
export function filterPanelEditor($q, uiSegmentSrv) { // eslint-disable-line no-unused-vars
  'use strict';
  return {
    restrict: 'E',
    scope: true,
    templateUrl: '/public/plugins/opennms-helm-app/panels/filter-panel/editor.html',
    controller: FilterPanelEditorCtrl,
  };
}
