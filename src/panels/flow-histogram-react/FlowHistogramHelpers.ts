// import { SelectableValue } from "@grafana/data";
import { FlowHistogramOptionsProps, DataProcessed } from "./FlowHistogramTypes";
import _ from 'lodash';
import moment from 'moment';
import { DataPosition } from "./FlowHistogramContants";

export const getUnits = (data, panelUnits) => {
    const dataElement = data[0];
    const toBits = dataElement && dataElement.meta && dataElement.meta.custom['toBits'] ? dataElement.meta.custom['toBits'] : null;
    let divisor = 1;
    let units;
    switch (panelUnits) {
        case 'b':
            units = toBits ? 'Bits' : 'Bytes';
            break;
        case 'kb':
            divisor = 1024;
            units = toBits ? 'Kb' : 'KB';
            break;
        case 'mb':
            divisor = 1024 ** 2;
            units = toBits ? 'Mb' : 'MB';
            break;
        case 'gb':
            divisor = 1024 ** 3;
            units = toBits ? 'Gb' : 'GB';
            break;
    }

    return {
        divisor: divisor,
        units: units,
    };
}

export const getFlowHistogramPlotData = (data: DataProcessed, options: { flowHistogramOptions: FlowHistogramOptionsProps }) => {

    switch (options.flowHistogramOptions.mode.label) {
        case 'Separate': {
            let inSeriesData: any = {
                label: 'In',
                bars: {
                    show: true,
                    barWidth: 0.2,                    
                    align: options.flowHistogramOptions.direction.label === 'Horizontal' ? 'left' : 'right'
                },
                data: data.inByLabel
            }

            let outSeriesData: any = {
                label: 'Out',
                bars: {
                    show: true,
                    barWidth: 0.2,                    
                    align: options.flowHistogramOptions.direction.label === 'Horizontal' ? 'right' : 'left'
                },
                data: data.outByLabel,
            }
            return [outSeriesData, inSeriesData]

        }
        case 'Stacked': {
            // let dataFromSeries = getLabeledValues(data)
            // let stackedSeriesData: any[] = [];
            // let seriesIndex = 0;
            // data?.series?
            // for (const key of Object.keys(dataFromSeries)) {
            //     let item: any = {
            //         label: key,
            //         // color: this.getColorForSeriesIndex(seriesIndex),
            //     };
            //     // if (!this.hiddenSeries[seriesIndex++]) {
            //         item.data = dataFromSeries[key];
            //     // }
            //     stackedSeriesData.push(item);
            // }

            return [];
        }
        default: return []
    }
}

export const getFlowHistogramPlotConfig = (data: DataProcessed, options: { flowHistogramOptions: FlowHistogramOptionsProps }) => {

    const stacked = options.flowHistogramOptions.mode.label === 'Stacked'
    const xaxis = {
        axisLabel: options.flowHistogramOptions.units.label
    }

    const yaxis = {
        mode: 'categories',
        tickLength: 0,
        ticks: data.indexedLabels,
        autoscaleMargin: 0.02,
    }

    const configOptions: any = {
        legend: {
            show: true,
            backgroundOpacity: 0
        },
        axisLabels: {
            show: true,
        },
        series: {
            bars: {
                align: "center",
                barWidth: 0.6,
                fill: 0.8,
                horizontal: options?.flowHistogramOptions?.direction?.label === 'Horizontal',
                lineWidth: 1,
                show: true,
                order: 1
            },
            stack: stacked,
        },
        grid: {
            borderWidth: 0,
        },
        xaxis: options.flowHistogramOptions.direction.label === 'Horizontal' ? xaxis : yaxis,
        yaxis: options.flowHistogramOptions.direction.label === 'Horizontal' ? yaxis : xaxis
    }



    return configOptions

}

export const getLabeledValues = (data, options: { flowHistogramOptions: FlowHistogramOptionsProps }): DataProcessed => {

    let labeledValues: DataProcessed = {
        inByLabel: [],
        outByLabel: [],
        indexedLabels: []
    }
    // I believe should be just one data series in this case since this work together with asTableSummary function
    if (data && data.series && data.series.length === 1) {
        const sd = data.series[0]
        const metric = sd && sd.meta && sd.meta.custom ? sd.meta.custom['metric'] : undefined;
        const inLabel = sd.meta.custom['toBits'] ? 'Bits In' : 'Bytes In'
        const outLabel = sd.meta.custom['toBits'] ? 'Bits Out' : 'Bytes Out'
        const inByData: any[] = getSeriesMetricValues(sd.fields, inLabel)
        const outByData: any[] = getSeriesMetricValues(sd.fields, outLabel)
        let inResult: any[] = [] // [bytes, metric]
        let outResult: any[] = [] // [bytes, metric]
        let metricLabels: any[] = []
        switch (metric) {
            case 'Applications':
                metricLabels = getSeriesMetricValues(sd.fields, 'Application')
                inResult = metricLabels.map((ml, idx) => { return [inByData[idx], ml] })
                outResult = metricLabels.map((ml, idx) => { return [outByData[idx], ml] })
                break
            case 'Hosts':
                metricLabels = getSeriesMetricValues(sd.fields, 'Host')
                inResult = metricLabels.map((ml, idx) => { return [inByData[idx], ml] })
                outResult = metricLabels.map((ml, idx) => { return [outByData[idx], ml] })
                break
            case 'Conversations':
                metricLabels = getSeriesMetricValues(sd.fields, 'Application')
                const sourceLabels = getSeriesMetricValues(sd.fields, 'Source')
                const destLabels = getSeriesMetricValues(sd.fields, 'Dest.')
                inResult = metricLabels.map((ml, idx) => {
                    return [inByData[idx], sourceLabels[idx] + ' <-> ' + destLabels[idx] + ' [' + ml + ']']
                })
                outResult = metricLabels.map((ml, idx) => {
                    return [outByData[idx], sourceLabels[idx] + ' <-> ' + destLabels[idx] + ' [' + (ml ? ml : 'Unknown') + ']']

                })
                break
        }

        if (!metric) {
            return {
                inByLabel: ['unknown'],
                outByLabel: ['unknown'],
                indexedLabels: []
            };
        }

        // Map the values to rates (average over the given time interval) if selected
        if (options.flowHistogramOptions.display.label === 'Rate') {
            let timeRangeInSeconds = moment.duration(data.timeRange.to.diff(data.timeRange.from)).asSeconds();
            inResult = inResult.map(pair => {
                return [pair[DataPosition.value] / timeRangeInSeconds, pair[DataPosition.metric]]
            })

            outResult = outResult.map(pair => {
                return [pair[DataPosition.value] / timeRangeInSeconds, pair[DataPosition.metric]]
            })
        }

        const indexedLabels = [...new Set(inResult.map((pair) => pair[DataPosition.metric])
            .concat(outResult.map((pair) => pair[DataPosition.metric])))]
            .sort()
            .map((label, idx) => [idx, label])

        const isHorizontal = options.flowHistogramOptions.direction.label === 'Horizontal'

        labeledValues = {
            inByLabel: swapLabelByIndex(inResult, indexedLabels, isHorizontal),
            outByLabel: swapLabelByIndex(outResult, indexedLabels, isHorizontal),
            indexedLabels: indexedLabels
        }
        return labeledValues

    } else {
        throw new Error('Only one query is permited in this panel');
    }
    return labeledValues
}

export const getSeriesMetricValues = (fields: any[], name: string) => {
    let match = fields.find(f => f.name === name)
    if (match && match.values) {
        return match.values.toArray()
    }
    else return []

}

/**
 * Convert the result data into a format that takes in consideration the ticks and graph direction 
 * if graph direction is horizontal then the data should be pairs [value, label_index] otherwise [label_index, value]
 * where label_index is the index associated to the category in indexedLabels (ticks) 
 * @param data 
 * @param indexedLabels 
 * @param isHorizontal 
 * @returns array data pairs []
 */
export const swapLabelByIndex = (data: any[], indexedLabels: any[], isHorizontal: boolean) => {
    return data.map(pair => {
        let match = indexedLabels.find(item => item[DataPosition.metric] === pair[DataPosition.metric])
        if (match) {
            let index = match[DataPosition.index]
            return isHorizontal ? [pair[DataPosition.value], index] : [index, pair[DataPosition.value]]
        } else return []
    })
}

