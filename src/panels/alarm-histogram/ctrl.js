import {MetricsPanelCtrl} from "app/plugins/sdk";
import _ from "lodash";
import $ from "jquery";
import "jquery.flot";
import "jquery.flot.selection";
import "jquery.flot.crosshair";
import "../../jquery.flot.categories";


class AlarmHistogramCtrl extends MetricsPanelCtrl {

    constructor($scope, $injector) {
        super($scope, $injector);

        this.scope = $scope;

        _.defaults(this.panel, {
            groupProperty: 'acknowledged',
            direction: 'horizontal',
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
                const counts = _.countBy(this.query(data, 'Acked By'), _.isNil);
                this.series = [
                    {
                        name: 'Outstanding',
                        count: _.defaultTo(counts[true], 0),
                        color: this.scope.$root.colors[0],
                    },
                    {
                        name: 'Acknowledged',
                        count: _.defaultTo(counts[false], 0),
                        color: this.scope.$root.colors[4],
                    },
                ];
                break;
            }

            case 'severity': {
                const counts = _.countBy(this.query(data, 'Severity'));
                this.series = [
                    {
                        name: 'Cleared',
                        count: _.defaultTo(counts['CLEARED'], 0),
                        color: '#EEE000',
                    },
                    {
                        name: 'Normal',
                        count: _.defaultTo(counts['NORMAL'], 0),
                        color: '#86B15B',
                    },
                    {
                        name: 'Indeterm.',
                        count: _.defaultTo(counts['INDETERMINATE'], 0),
                        color: '#990000',
                    },
                    {
                        name: 'Warning',
                        count: _.defaultTo(counts['WARNING'], 0),
                        color: '#FCCC3B',
                    },
                    {
                        name: 'Minor',
                        count: _.defaultTo(counts['MINOR'], 0),
                        color: '#EE901C',
                    },
                    {
                        name: 'Major',
                        count: _.defaultTo(counts['MAJOR'], 0),
                        color: '#E3692F',
                    },
                    {
                        name: 'Critical',
                        count: _.defaultTo(counts['CRITICAL'], 0),
                        color: '#DB4345',
                    },

                ];
                break;
            }
        }

        this.render();
    }

    onDataError() {
        this.series = [];
        this.render();
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

        // Draw graph
        switch (this.panel.direction) {
            case 'horizontal':
                $.plot(this.elem,
                    _.map(this.series, function (serie) {
                        return {
                            data: [[serie.count, serie.name]],
                            color: serie.color,
                        };
                    }),
                    {
                        series: {
                            bars: {
                                show: true,
                                barWidth: 0.6,
                                align: "center",
                                fill: 0.8,
                                lineWidth: 1.0,
                                horizontal: true,
                            }
                        },
                        yaxis: {
                            mode: "categories",
                            tickLength: 0,
                            autoscaleMargin: .02,
                        },
                        grid: {
                            borderWidth: 0,
                        },
                    });
                break;

            case 'vertical':
                $.plot(this.elem,
                    _.map(this.series, function (serie) {
                        return {
                            data: [[serie.name, serie.count]],
                            color: serie.color,
                        };
                    }),
                    {
                        series: {
                            bars: {
                                show: true,
                                barWidth: 0.5,
                                align: "center",
                                fill: 0.8,
                                lineWidth: 1.0,
                                horizontal: false,
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
                    });
                break;
        }
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
