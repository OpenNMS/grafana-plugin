import {MetricsPanelCtrl} from "app/plugins/sdk";
import _ from "lodash";
import $ from "jquery";
import moment from "moment";
import "jquery.flot";
import "jquery.flot.time";
import "jquery.flot.selection";
import "jquery.flot.crosshair";
import "jquery.flot.stack";
import "flot-axislabels/jquery.flot.axislabels";
import "flot/jquery.flot.categories";

class HelmHistogramCtrl extends MetricsPanelCtrl {
    /** @ngInject */
    constructor($scope, $injector, $timeout) {
        super($scope, $injector);

        this.scope = $scope;
        this.$timeout = $timeout;

        // We use both the 'stack' and 'categories' Flot plugins
        // For these to work well together, we need the 'categories' plugin
        // to be called *before* the stack plugin.
        // Re-order them if necessary
        const categoriesPluginIdx = _.findIndex($.plot.plugins, plugin => {
            return plugin.name === 'categories';
        });
        const stackPluginIdx = _.findIndex($.plot.plugins, plugin => {
            return plugin.name === 'stack';
        });
        if (categoriesPluginIdx >= 0 && stackPluginIdx >= 0 && categoriesPluginIdx > stackPluginIdx) {
            // We found both plugins, and the categories plugin comes *after* the stack plugin, swap them
            const stackPlugin = $.plot.plugins[stackPluginIdx];
            $.plot.plugins[stackPluginIdx] = $.plot.plugins[categoriesPluginIdx];
            $.plot.plugins[categoriesPluginIdx] = stackPlugin;
        }

        this._renderRetries = 0;

        _.defaults(this.panel, {
            direction: 'horizontal',
            units: 'b',
            display: 'total',
            mode: 'separate',
            legendPosition: 'right'
        });

        this.retryTimes = 10; // number of times to retry
        this.retryDelay = 100; // milliseconds, how long to wait to retry

        this.events.on('init-edit-mode', this.onInitEditMode.bind(this));
        this.events.on('data-received', this.onDataReceived.bind(this));
        this.events.on('data-error', this.onDataError.bind(this));
        this.events.on('data-snapshot-load', this.onDataReceived.bind(this));
        this.events.on('render', this.onRender.bind(this));
    }

    link($scope, elem, attrs, ctrl) {
        this.elem = elem.find('.histogram-chart');
        this.legendContainer = elem.find('#legend-container');
        this.wrapper = elem.find('.graph-canvas-wrapper');
        this.ctrl = ctrl;
    }

    onInitEditMode() {
        this.addEditorTab('Grouping', 'public/plugins/opennms-helm-app/panels/flow-histogram/editor.html', 2);
    }

    onDataReceived(data) {
        // Adjust the values based on the units selected
        const unitInfo = HelmHistogramCtrl.getUnits(data, this.panel.units);
        this.units = unitInfo.units;

        const labeledValues = this.getLabeledValues(data);
        this.series = this.getSeries(labeledValues, unitInfo.divisor);

        this.render();
    }

    onDataError() {
        this.series = [];
        this.render();
    }

    onRender() {
        this.elem.empty();
        if (this.legendContainer) {
            this.legendContainer.empty();
        }

        let height = this.ctrl.height || this.ctrl.panel.height || (this.ctrl.row && this.ctrl.row.height);
        if (_.isString(height)) {
            height = parseInt(height.replace('px', ''), 10);
        }

        if (this.elem.width() === 0 || height === 0 || height === undefined) {
            if (this._renderRetries > this.retryTimes) {
                console.log('onRender: still unable to determine height, and we ran out of retries');
                return false;
            }
            this._renderRetries++;

            console.log('onRender: unable to determine height, retrying again in ' + this.retryDelay + 'ms');
            this.$timeout(() => {
                this.onRender();
            }, this.retryDelay);
            return true;
        }

        height -= 5; // padding
        height -= this.ctrl.panel.title ? 24 : 9; // subtract panel title bar

        // Move the legend depending on which position was selected
        // Note: not sure if this is the best way to be doing this
        switch (this.panel.legendPosition) {
            case 'right':
                this.legendContainer.css('height', height + 'px');
                this.legendContainer.removeClass('legend-container-bottom');
                this.legendContainer.addClass('legend-container-right');
                this.wrapper.css('display', 'flex');
                this.elem.css('width', '100%');
                break;
            case 'bottom':
                this.legendContainer.css('height', '');
                this.legendContainer.removeClass('legend-container-right');
                this.legendContainer.addClass('legend-container-bottom');
                this.wrapper.css('display', '');
                this.elem.css('width', '');
                height -= this.legendContainer.height();
                break;
        }

        this.elem.css('height', height + 'px');

        const seriesData = this.getSeriesData(this.series, this.panel.direction, this.panel.mode);
        const options = this.getOptions(seriesData);
        $.plot(this.elem, seriesData, options);
    }

    getLabeledValues(data) {
        const dataElement = data[0];
        let labelFunc;

        switch (dataElement.vars.metric) {
            case 'applications':
                labelFunc = (column, row) => {
                    return row[column.indexOf('application')];
                };
                break;
            case 'hosts':
                labelFunc = (column, row) => {
                    return row[column.indexOf('host')];
                };
                break;
            case 'conversations':
                labelFunc = (column, row) => {
                    return row[column.indexOf('Source IP')] + ' <-> ' +
                        row[column.indexOf('Dest. IP')] + ' [' + row[column.indexOf('Application')] + ']';
                };
                break;
        }

        let inByLabel = HelmHistogramCtrl.extractValueAndLabel(data, dataElement.vars.toBits ? 'Bits In' : 'Bytes In',
            'In', labelFunc);
        let outByLabel = HelmHistogramCtrl.extractValueAndLabel(data, dataElement.vars.toBits ? 'Bits Out' : 'Bytes Out',
            'Out', labelFunc);

        // Map the values to rates (average over the given time interval) if selected
        if (this.panel.display === 'rate') {
            this.units += '/s';
            let timeRangeInSeconds = moment.duration(this.range.to.diff(this.range.from)).asSeconds();
            inByLabel = _.map(inByLabel, (element) => {
                return {
                    key: element.key,
                    value: element.value / timeRangeInSeconds
                };
            });

            outByLabel = _.map(outByLabel, (element) => {
                return {
                    key: element.key,
                    value: element.value / timeRangeInSeconds
                };
            });
        }

        return {
            inByLabel: inByLabel,
            outByLabel: outByLabel
        }
    }

    getSeries(values, divisor) {
        const inColor = '#86B15B';
        const outColor = '#DB4345';

        let series = [];

        switch (this.panel.direction) {
            case 'horizontal':
                for (let i = values.inByLabel.length - 1; i >= 0; i--) {
                    series.push(HelmHistogramCtrl.generateResultObject(values.outByLabel, i, divisor, outColor));
                    series.push(HelmHistogramCtrl.generateResultObject(values.inByLabel, i, divisor, inColor));
                }
                break;
            case 'vertical':
                for (let i = 0; i < values.inByLabel.length; i++) {
                    series.push(HelmHistogramCtrl.generateResultObject(values.inByLabel, i, divisor, inColor));
                    series.push(HelmHistogramCtrl.generateResultObject(values.outByLabel, i, divisor, outColor));

                }
                break;
        }

        return series;
    }

    getOptions(seriesData) {
        // Set up the graph settings
        const series = {
            bars: {
                show: true,
                barWidth: 0.6,
                align: "center",
                fill: 0.8,
                lineWidth: 1.0
            },
            stack: this.panel.mode === 'stacked'
        };

        const axisSettings = {
            mode: "categories",
            tickLength: 0,
            autoscaleMargin: .02
        };

        const axisLabelSetting = [{
            axisLabel: this.units
        }];

        const classForLabels = this.panel.legendPosition === 'bottom' ? 'flow-histogram-label-bottom' :
            'flow-histogram-label-right';
        const labelFormatterFunc = (label, series) => {
            return '<span class="' + classForLabels + '">' + label + '</span>';
        };

        const options = {
            legend: {
                show: true,
                sorted: this.panel.mode === 'stacked' ? null : 'ascending',
                container: this.legendContainer,
                noColumns: this.panel.legendPosition === 'bottom' ? seriesData.length : 1,
                labelBoxBorderColor: null,
                labelFormatter: labelFormatterFunc
            },
            axisLabels: {
                show: true
            },
            series: series,
            grid: {
                borderWidth: 0,
            }
        };

        switch (this.panel.direction) {
            case 'horizontal':
                options.xaxes = axisLabelSetting;
                series.bars.horizontal = true;
                options.yaxis = axisSettings;
                break;

            case 'vertical':
                options.yaxes = axisLabelSetting;
                series.bars.horizontal = false;
                options.xaxis = axisSettings;
                break;
        }

        return options;
    }

    getSeriesData(series, direction, mode) {
        let dataFromSeries;

        switch (mode) {
            case 'separate': {
                dataFromSeries = HelmHistogramCtrl.getData(this.series, this.panel.direction);
                let inSeriesData = {
                    label: "In",
                    data: dataFromSeries.dataIn,
                    bars: {
                        show: true,
                        barWidth: 0.2,
                    },
                    color: '#86B15B'
                };

                let outSeriesData = {
                    label: "Out",
                    data: dataFromSeries.dataOut,
                    bars: {
                        show: true,
                        barWidth: 0.2,
                    },
                    color: '#DB4345'
                };
                switch (direction) {
                    case 'horizontal':
                        inSeriesData.bars.align = "left";
                        outSeriesData.bars.align = "right";
                        return [
                            inSeriesData,
                            outSeriesData
                        ];
                    case 'vertical':
                        inSeriesData.bars.align = "right";
                        outSeriesData.bars.align = "left";
                        return [
                            outSeriesData,
                            inSeriesData
                        ];
                }
                break;
            }
            case 'stacked': {
                dataFromSeries = HelmHistogramCtrl.getDataStacked(this.series, this.panel.direction);
                let stackedSeriesData = [];
                for (const key of Object.keys(dataFromSeries)) {
                    stackedSeriesData.push({
                        label: key,
                        data: dataFromSeries[key]
                    });
                }

                return stackedSeriesData;
            }
        }
    }

    static getData(series, direction) {
        let dataIn = [];
        let dataOut = [];

        switch (direction) {
            case 'horizontal':
                for (let i = 0, j = 1; i < series.length - 1; i += 2, j += 2) {
                    dataOut.push([series[i].count, series[i].name]);
                    dataIn.push([series[j].count, series[j].name]);
                }
                break;
            case 'vertical':
                for (let i = 0, j = 1; i < series.length - 1; i += 2, j += 2) {
                    dataIn.push([series[i].name, series[i].count]);
                    dataOut.push([series[j].name, series[j].count]);
                }
                break;
        }

        return {
            dataIn: dataIn,
            dataOut: dataOut
        };
    }

    static getDataStacked(series, direction) {
        let perHostData = {};

        switch (direction) {
            case 'horizontal':
                for (let i = series.length - 2, j = series.length - 1; i >= 0; i -= 2, j -= 2) {
                    perHostData[series[i].name] = [];
                    perHostData[series[i].name].push([series[i].count, 'Out']);
                    perHostData[series[i].name].push([series[j].count, 'In']);
                }
                break;
            case 'vertical':
                for (let i = 0, j = 1; i < series.length - 1; i += 2, j += 2) {
                    perHostData[series[i].name] = [];
                    perHostData[series[i].name].push(['In', series[i].count]);
                    perHostData[series[i].name].push(['Out', series[j].count]);
                }
                break;
        }

        return perHostData;
    }

    static extractValueAndLabel(data, valueColumn, direction, labelFunc) {
        const values = [];

        const columns = data[0].columns.map((e) => {
            return e.text;
        });
        const valueColumnIndex = columns.indexOf(valueColumn);

        data[0].rows.forEach((row) => {
            values.push({
                key: labelFunc(columns, row),
                value: row[valueColumnIndex]
            });
        });

        return values;
    }

    static getUnits(data, panelUnits) {
        const dataElement = data[0];
        let divisor = 1;
        let units;
        switch (panelUnits) {
            case 'b':
                units = dataElement.vars.toBits ? "Bits" : "Bytes";
                break;
            case 'kb':
                divisor = 1024;
                units = dataElement.vars.toBits ? "Kb" : "KB";
                break;
            case 'mb':
                divisor = 1024 ** 2;
                units = dataElement.vars.toBits ? "Mb" : "MB";
                break;
            case 'gb':
                divisor = 1024 ** 3;
                units = dataElement.vars.toBits ? "Gb" : "GB";
                break;
        }

        return {
            divisor: divisor,
            units: units
        }
    }

    static generateResultObject(data, i, divisor, color) {
        return {
            name: data[i].key,
            count: _.defaultTo(data[i].value, 0) / divisor,
            color: color,
        }
    }
}

HelmHistogramCtrl.templateUrl = 'panels/flow-histogram/module.html';

export {
    HelmHistogramCtrl
};
