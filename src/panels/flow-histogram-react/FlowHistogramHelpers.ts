// import { SelectableValue } from "@grafana/data";
import { FlowHistogramOptionsProps, FlowPanelDataProcessed, FlowPanelUnitInfo } from "./FlowHistogramTypes";
import _ from 'lodash';
import moment from 'moment';
import { DataPosition, FLowDataDirection, UnitInfo } from "./FlowHistogramContants";

export const getFlowHistogramPlotData = (processedData: FlowPanelDataProcessed, options: { flowHistogramOptions: FlowHistogramOptionsProps }): any[] => {

    switch (options.flowHistogramOptions.mode.label) {
        case 'Separate': {
            let inSeriesData: any = {
                label: 'In',
                bars: {
                    show: true,
                    barWidth: 0.2,
                    align: options.flowHistogramOptions.direction.label === 'Horizontal' ? 'left' : 'right'
                },
                data: processedData.separateData?.inByLabel
            }

            let outSeriesData: any = {
                label: 'Out',
                bars: {
                    show: true,
                    barWidth: 0.2,
                    align: options.flowHistogramOptions.direction.label === 'Horizontal' ? 'right' : 'left'
                },
                data: processedData.separateData?.outByLabel,
            }
            return [outSeriesData, inSeriesData]

        }
        case 'Stacked': {
            return processedData.stackedData?.metricsByLabel ? processedData.stackedData.metricsByLabel : []
        }
        default: return []
    }
}

export const getFlowHistogramPlotConfig = (processedData: FlowPanelDataProcessed, options: { flowHistogramOptions: FlowHistogramOptionsProps }) => {

    const stacked = options.flowHistogramOptions.mode.label === 'Stacked'
    const horizontal = options.flowHistogramOptions.direction.label === 'Horizontal'
    const container = options.flowHistogramOptions.position.label === 'Under Graph' ? $('.flow-histogram-legend-bottom') : $('.flow-histogram-legend-right') 
    const noColumns = options.flowHistogramOptions.position.label === 'Under Graph' ? 10 : 2
    const showLegend = options.flowHistogramOptions.showLegend
    const legendPosition = options.flowHistogramOptions.position.value 
    
    const yaxis = {
        mode: 'categories',
        tickLength: 0,
        ticks: stacked ? processedData.stackedData?.ticks : processedData.separateData?.ticks,
        autoscaleMargin: 0.02,
    }

    const configOptions: any = {
        legend: {
            show: showLegend,
            container: container,
            position: legendPosition,
            backgroundOpacity: 0,
            noColumns: noColumns,
            placement: container ? 'outsideGrid' : null,
            labelFormatter: (label, series) => {return '<a style="margin:3px" >' + label + '</a>'; }
        },        
        series: {
            bars: {
                align: "center",
                barWidth: 0.6,
                fill: 0.8,
                horizontal: horizontal,
                lineWidth: 1,
                show: true,
            },
            stack: stacked,
        },
        grid: {
            borderWidth: 0,
        },
        xaxis: horizontal ? {} : yaxis,
        yaxis: horizontal ? yaxis : {}
    }
    return configOptions

}

export const getLabeledValues = (data, options: { flowHistogramOptions: FlowHistogramOptionsProps }): FlowPanelDataProcessed => {

    let labeledValues: FlowPanelDataProcessed = {}
    const unitInfo: FlowPanelUnitInfo = UnitInfo(options, data?.series)

    // I believe should be just one data series in this case since this work together with asTableSummary function
    if (data && data.series && data.series.length === 1) {
        const sd = data.series[0]
        const metric = sd && sd.meta && sd.meta.custom ? sd.meta.custom['metric'] : undefined;
        const stacked = options.flowHistogramOptions.mode.label === 'Stacked'
        const horizontal = options.flowHistogramOptions.direction.label === 'Horizontal'
        const rate = options.flowHistogramOptions.display.label === 'Rate'
        const toBits = sd.meta.custom['toBits'] ? true : false
        const inLabel = toBits ? 'Bits In' : 'Bytes In'
        const outLabel = toBits ? 'Bits Out' : 'Bytes Out'
        const inByData: any[] = getSeriesMetricValues(sd.fields, inLabel)
        const outByData: any[] = getSeriesMetricValues(sd.fields, outLabel)

        let inResult: any[] = []// [bytes, metric]
        let outResult: any[] = [] // [bytes, metric]
        let stackedResult: any[] = []
        let metricLabels: any[] = []

        if (!metric) {
            return labeledValues;
        }

        let labelFunction
        switch (metric) {
            case 'Applications':
                metricLabels = getSeriesMetricValues(sd.fields, 'Application')
                labelFunction = name => name
                break
            case 'Hosts':
                metricLabels = getSeriesMetricValues(sd.fields, 'Host')
                labelFunction = name => name
                break
            case 'Conversations':
                metricLabels = getSeriesMetricValues(sd.fields, 'Application')
                const sourceLabels = getSeriesMetricValues(sd.fields, 'Source')
                const destLabels = getSeriesMetricValues(sd.fields, 'Dest.')
                labelFunction = (name: string, idx: number): string => {
                    return sourceLabels[idx] + ' <-> ' + destLabels[idx] + ' [' + (name ? name : 'Unknown') + ']'
                }
                break
        }

        const indexedMetricLabels = getIndexedMetricLabels(metricLabels, labelFunction, stacked)
        const timeRangeInSeconds = moment.duration(data.timeRange.to.diff(data.timeRange.from)).asSeconds();

        if (!stacked) {
            inResult = getSeparatedResultData(inByData, indexedMetricLabels, horizontal, unitInfo, rate, timeRangeInSeconds)
            outResult = getSeparatedResultData(outByData, indexedMetricLabels, horizontal, unitInfo, rate, timeRangeInSeconds)

        } else {
            stackedResult = getStackedResultData(indexedMetricLabels, inByData, outByData, horizontal, unitInfo, rate, timeRangeInSeconds)
        }

        labeledValues = {
            separateData: {
                inByLabel: inResult,
                outByLabel: outResult,
                ticks: indexedMetricLabels
            },
            stackedData: {
                metricsByLabel: stackedResult,
                ticks: [[FLowDataDirection.dataIn.value, FLowDataDirection.dataIn.label], [FLowDataDirection.dataOut.value, FLowDataDirection.dataOut.label]]
            }
        }
        return labeledValues

    } else {
        throw new Error('Only one query is permited in this panel');
    }
    return labeledValues
}

/**
 * Returns and array of [index, label] 
 * where index is the record position in the original data and the label to be displayed in the graph
 * @param metricLabels 
 * @param labelFunction 
 * @param stacked 
 * @returns 
 */
export const getIndexedMetricLabels = (metricLabels: any[], labelFunction, stacked: boolean) => {
    return metricLabels.map((label, idx) => [idx, labelFunction(label, idx)])
}

/**
 * Gets the final data formatted to be passed into the plot function 
 * based in graph direction (vertical, horizontal) and mode (separated or stacked)
 * @param data 
 * @param indexedLabels 
 * @param horizontal 
 * @param stacked 
 * @returns 
 */
export const getSeparatedResultData = (data: any[], indexedLabels: any[], horizontal: boolean, unitInfo: FlowPanelUnitInfo, rate: boolean, timeRangeInSeconds: number) => {
    const divisor = rate ? unitInfo.divisor * timeRangeInSeconds : unitInfo.divisor
    return indexedLabels.map((il) => {
        const index = il[DataPosition.indexLabel.index]
        const value = data[index] / divisor
        return horizontal ? [value, index] : [index, value]
    })
}

export const getStackedResultData = (indexedMetricLabels: any[], inByData: any[], outByData: any[], horizontal: boolean, unitInfo: FlowPanelUnitInfo, rate: boolean, timeRangeInSeconds: number): any[] => {
    const divisor = rate ? unitInfo.divisor * timeRangeInSeconds : unitInfo.divisor
    return indexedMetricLabels.map((imLabel) => {
        const index = imLabel[DataPosition.indexLabel.index]
        const label = imLabel[DataPosition.indexLabel.label]
        const inData = inByData[index] / divisor
        const outData = outByData[index] / divisor
        return horizontal ?
            { label: label, data: [[inData, FLowDataDirection.dataIn.value], [outData, FLowDataDirection.dataOut.value]] } :
            { label: label, data: [[FLowDataDirection.dataIn.value, inData], [FLowDataDirection.dataOut.value, outData]] }

    })
}


export const getSeriesMetricValues = (fields: any[], name: string) => {
    let match = fields.find(f => f.name === name)
    if (match && match.values) {
        return match.values.toArray()
    }
    else { return [] }

}

