import {MetricsPanelCtrl} from  'app/plugins/sdk';

import _ from 'lodash';

import 'jquery.flot';
import 'jquery.flot.selection';
import 'jquery.flot.crosshair';

import '../../jquery.flot.categories';


class AlarmHistogramCtrl extends MetricsPanelCtrl {

    constructor($scope, $injector) {
        super($scope, $injector);

        this.scope = $scope;

        _.defaults(this.panel,  {
            groupProperty: 'acknowledged',
        });

        this.events.on('init-edit-mode', this.onInitEditMode.bind(this));
        this.events.on('data-received', this.onDataReceived.bind(this));
        this.events.on('data-error', this.onDataError.bind(this));
        this.events.on('data-snapshot-load', this.onDataReceived.bind(this));
        this.events.on('render', this.onRender.bind(this));
    }

    link($scope, elem, attrs, ctrl) {
        this.elem = elem.find('.histogram-chart');
        this.ctrl = ctrl;
    }

    onInitEditMode() {
        this.addEditorTab('Grouping', 'public/plugins/opennms-helm-app/panels/alarm-histogram/editor.html', 2);
    }

    onDataReceived(data) {
        switch (this.panel.groupProperty) {
            case 'acknowledged': {
                const counts = _.countBy(this.query(data, 'AckedBy'), _.isNil);
                this.series = [
                    {
                        name: "Pending",
                        count: _.defaultTo(counts[true], 0),
                    },
                    {
                        name: "Acknowledged",
                        count: _.defaultTo(counts[false], 0),
                    },
                ];
                break;
            }

            case 'severity': {
                const counts = _.countBy(this.query(data, 'Severity'));
                this.series = _.map(counts, function (count, group) {
                    return {
                        name: group,
                        count: count,
                    }
                });
                break;
            }
        }

        this.render(this.series);
    }

    onDataError() {
        this.series = [];
        this.render(this.series);
    }

    onRender() {
        // Size handling
        if (this.elem.width() === 0) {
            return true;
        }

        let height = this.ctrl.height || this.ctrl.panel.height || this.ctrl.row.height;
        if (_.isString(height)) {
            height = parseInt(height.replace('px', ''), 10);
        }
        height -= 5; // padding
        height -= this.ctrl.panel.title ? 24 : 9; // subtract panel title bar

        this.elem.css('height', height + 'px');

        // Convert data to series
        const data = _.map(this.series, function(serie) { return [serie.name, serie.count]; });

        // Draw graph
        $.plot(this.elem, [data], {
            series: {
                bars: {
                    show: true,
                    barWidth: 0.5,
                    align: "center",
                    fill: true,
                    lineWidth: 0,
                }
            },
            xaxis: {
                mode: "categories",
                tickLength: 0,
                autoscaleMargin: .02,
            },
            grid: {
                borderWidth: 0,
            },
            colors: this.scope.$root.colors,
        });
    }

    query(data, column) {
        // TODO: Make this a generator to save memory
        let result = [];

        for (let i = 0; i < data.length; i++) {
            let columnIndex = _.findIndex(data[i].columns, {text: column});
            for (let j = 0; j < data[i].rows.length; j++) {
                result.push(data[i].rows[j][columnIndex]);
            }
        }

        return result;
    }
}

AlarmHistogramCtrl.templateUrl = 'panels/alarm-histogram/module.html';

export {
    AlarmHistogramCtrl
};
