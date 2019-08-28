import {MetricsPanelCtrl} from "app/plugins/sdk";
import {filterPanelEditor} from './editor';
import _ from "lodash";

import {FilterColumn} from '../../lib/filter_column';

class FilterCtrl extends MetricsPanelCtrl {
    /** @ngInject */
    constructor($scope, $q, $injector, datasourceSrv, templateSrv, variableSrv, timeSrv) {
        super($scope, $injector);

        this.datasourceSrv = datasourceSrv;
        this.templateSrv = templateSrv;
        this.variableSrv = variableSrv;
        this.timeSrv = timeSrv;
        this.$q = $q;

        _.defaults(this.panel, {
            columns: []
        });
        this.columnData = {};

        if (this.panel.datasources) {
            // upgrade existing column format
            for (const col of this.panel.columns) {
                if (this.panel.selected && this.panel.selected[col.label]) {
                    col.selected = this.panel.selected[col.label];
                }
                if (this.panel.inputTypes && this.panel.inputTypes[col.label]) {
                    col.inputType = this.panel.inputTypes[col.label];
                }
                if (this.panel.datasources && this.panel.datasources[col.label]) {
                    col.datasource = this.panel.datasources[col.label];
                }
            }
            delete this.panel.selected;
            delete this.panel.inputTypes;
            delete this.panel.datasources;
        }

        this.$scope.dashboard = this.dashboard;
        this.$scope.ctrl = this;
        this.$scope.columnVariables = [];
        this.panel.columns = this.panel.columns.map((col) => FilterColumn.fromJSON(col));

        this.events.on('init-edit-mode', this.onInitEditMode.bind(this));
        this.events.on('render', this.onRender.bind(this));
        this.render();
    }

    link($scope, elem, attrs, ctrl) {
        this.ctrl = ctrl;
        this.$scope = $scope;
    }

    onInitEditMode() {
        this.addEditorTab('Filtering', filterPanelEditor, 2);
    }

    onRender() {
        this.$scope.columns = this.panel.columns.map(column => this.enrichColumn(column));
        this.updateVariables();
    }

    updateVariables() {
        return this.$q.all(this.$scope.columns.map((column, index) => this.getVariable(column, index))).then(cols => {
            this.$scope.columnVariables = cols;
            return cols;
        });
    }

    variableChanged(col) {
        if (col && col.text) {
            this.updateVariables().then(() => {
                this.render();
            });
        }
    }

    enrichColumn(obj) {
        return new FilterColumn(obj.text, obj.label, obj.datasource, obj.resource, obj.inputType, obj.entityType, obj.id, obj.selected);
    }

    getVariable(column /*, index */) {
        const label = column.text;
        const resource = column.resource;

        const c = Object.assign({}, column);
        c.type = column.inputType === 'text' ? 'textbox' : 'query';
        c.multi = column.inputType === 'multi' || column.inputType === undefined;
        const query = this.variableSrv.createVariableFromModel(c);

        const selected = column.selected;
        if (selected) {
            query.options.forEach(opt => {
                if (Array.isArray(selected.value)) {
                    opt.selected = selected.value.contains(opt.value);
                }
            });
            query.current = selected;
            if (query.current) {
                query.current.resource = resource;
            }
        }

        query.datasource = column.datasource || this.datasource.name;
        query.multi = column.inputType === 'multi' || column.inputType === undefined;
        query.inputType = column.inputType;
        query.entityType = column.entityType;

        if (column.inputType === 'text') {
            if (selected && selected.value !== null && selected.value !== undefined) {
                query.query = selected.value;
            }
        } else {
            query.includeAll = true;
            query.query = resource;

            // if this is an entity query, wrap in the entity type
            if (query.entityType && query.entityType.id) {
                query.query = column.entityType.queryFunction + '(' + resource + ')';
            }
        }

        query.label = label;
        query.resource = resource;
        query.id = column.id;

        // console.log('getVariable: querying:', query);
        return query.updateOptions().then(function() {
            return query;
        });
    }

    variableUpdated(variable, index) {
        const self = this;

        variable.updateOptions().then(() => {
            if (variable.current) {
                variable.current.resource = variable.resource;
                variable.current.datasource = variable.datasource;
                variable.current.inputType = variable.inputType;
                variable.current.entityType = variable.entityType;
            }
            this.panel.columns[index].selected = variable.current;
            self.dashboard.panels.forEach(panel => {
                if (panel !== self.panel) {
                    panel.refresh();
                }
            });
        });
    }
}

FilterCtrl.templateUrl = 'panels/filter-panel/module.html';

export {
    FilterCtrl
};
