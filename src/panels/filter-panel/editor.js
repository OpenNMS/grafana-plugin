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

  setDatasource(dsName) {
    const self = this;
    console.debug('Setting datasource to: ' + dsName);
    return self.getDatasource(dsName).then((ds) => {
      self.$scope.current.datasourceType = ds.type;
      return ds;
    }).catch((err) => {
      console.warn('Failed to get datasource ' + dsName, err);
      self.$scope.current.datasourceType = undefined;
      return self.$q.reject(err);
    });
  }

  setEntityType(type) {
    const t = type ? type : this.entityTypes[0];
    this.$scope.current.entityType = t;
    console.debug('Setting entity type to:', t);
  }

  updateCurrent() {
    const self = this;
    const deferred = self.$q.defer();

    self.$scope.$evalAsync(() => {
      const current = self.$scope.current;
      const dsName = current.datasource && current.datasource.name ? current.datasource.name : undefined;

      self.setDatasource(dsName).then(() => {
        self.setEntityType(current.entityType);
        deferred.resolve(self.render());
      }).catch((err) => {
        deferred.reject(err);
      });
    });
    return deferred.promise;
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
    const self = this;
    const $scope = self.$scope;
    const dsName = $scope.datasource;

    return self.getDatasource(dsName).then((ds) => {
      const deferred = self.$q.defer();

      // make sure the other scoped variables are updated before evaluating everything else
      $scope.$evalAsync(() => {
        const entityType = ds.type === 'opennms-helm-entity-datasource' ? $scope.current.entityType : undefined;

        const opts = {
          queryType: 'attributes'
        };
        if (entityType) {
          opts.entityType = entityType.id;
        }
    
        ds.metricFindQuery(entityType ? entityType.queryFunction + '()' : null, opts).then((res) => {
          console.debug('getColumnOptions: metricFindQuery result:', res);
    
          // filter out columns that have already been selected
          const filtered = res.filter(a => self.panel.columns.indexOf(a) < 0);
      
          const segments = filtered.map(c => self.uiSegmentSrv.newSegment({
            // datasource: ds.name,
            value: c.name
          }));
      
          deferred.resolve(segments);
        }).catch((err) => {
          deferred.reject(err);
        });
      });

      return deferred.promise;
    });
  }

  addColumn() {
    const self = this;
    const $scope = self.$scope;
    const label = self.addColumnSegment.value;

    return self.getDatasource($scope.current.datasource).then((ds) => {
      const deferred = self.$q.defer();

      // make sure the other scoped variables are updated before evaluating everything else
      $scope.$evalAsync(() => {
        const entityType = ds.type === 'opennms-helm-entity-datasource' ? $scope.current.entityType : undefined;

        const opts = {
          queryType: 'attributes'
        };
        if (entityType) {
          opts.entityType = entityType.id;
        }
    
        ds.metricFindQuery(entityType ? entityType.queryFunction + '()' : null, opts).then((res) => {
          console.debug('addColumn: metricFindQuery result:', res);
    
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
          deferred.resolve(self.render());
        }).catch((err) => {
          deferred.reject(err);
        });
      });

      return deferred.promise;
    });
  }

  render() {
    this.panelCtrl.render();
  }

  getDatasource(ds) {
    const dsName = ds && ds.name ? ds.name : ds;
    return this.datasourceSrv.get(dsName).then((d) => {
      console.debug('getDatasource: ' + (dsName ? dsName : 'default') + '=', d);
      return d;
    });
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

  reset() {
    const self = this;
    self.$scope.datasources = self.getDatasources();
    const current = self.$scope.current;

    return self.getDatasource(self.panel.datasource).then((ds) => {
      const datasource = self.$scope.datasources.filter((existing) => {
        return ds.name === existing.name;
      })[0];
      console.debug('reset(): panel datasource "' + self.panel.datasource + '" matched:', datasource);
      current.datasource = datasource;
      current.entityType = undefined;
      return self.updateCurrent();
    });
  }

  removeColumn(column, index) {
    console.debug('removing column:', column);
    this.panel.columns.splice(index, 1);
    return this.reset();
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
