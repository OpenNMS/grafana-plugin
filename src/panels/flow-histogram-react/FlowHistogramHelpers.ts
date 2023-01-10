import { SelectableValue } from "@grafana/data";
import { FlowHistogramOptionsProps } from "./FlowHistogramTypes";

const formatFlowHistogram = (histogramUnit: SelectableValue<string>, v) => {
    let divisor = 1;

    if (histogramUnit.label === 'KB') {
        divisor = 1000
    } else if (histogramUnit.label === 'MB') {
        divisor = 1000000
    } else if (histogramUnit.label === 'GB') {
        divisor = 1000000000;
    }

    return v / divisor
}
export const getFlowHistogramPlotData = (data, options: { flowHistogramOptions: FlowHistogramOptionsProps }) => {

    let totalIn = 0;
    let totalOut = 0;
    let countedIn = 0;
    let countedOut = 0;
    data?.series?.forEach((sd) => {
        let counterType;
        if (sd.name.includes('(In)')) {
            counterType = 'in'
        } else if (sd.name.includes('(Out)')) {
            counterType = 'out'
        }
        if (counterType) {
            const valueField = sd.fields.find((f) => f.name === 'Value')
            if (valueField) {
                const valueArray = valueField.values.toArray()
                valueArray.forEach((v) => {
                    // OPTIONS LINK: Units. Convert Bytes to selected option.
                    let formatted = formatFlowHistogram(options.flowHistogramOptions.units, v);
                    if (counterType === 'in') {
                        countedIn += 1;
                        totalIn += formatted
                    } else if (counterType === 'out') {
                        totalOut += formatted
                        countedOut += 1;
                    }
                })
            }
        }
    })

    // OPTIONS LINK: Display. Leave alone if Total, if Rate, divide by number of entries.
    if (options.flowHistogramOptions.display.label === 'Rate') {
        totalIn = totalIn / countedIn
        totalOut = totalOut / countedOut
    }
    const inData = options.flowHistogramOptions.direction.label === 'Horizontal' ? [totalIn, 0] : [0, totalIn]
    const stackedIndex = options.flowHistogramOptions.mode.label === 'Separate' ? 1 : 0;
    const outData = options.flowHistogramOptions.direction.label === 'Horizontal' ? [totalOut, stackedIndex] : [stackedIndex, totalOut]
    return [
        { label: 'Out', data: [outData] },
        { label: 'In', data: [inData] },
    ]
}

export const getFlowHistogramPlotConfig = (options: { flowHistogramOptions: FlowHistogramOptionsProps }) => {
    const axis = options.flowHistogramOptions.direction.label === 'Horizontal' ? 'yaxis' : 'xaxis'
    let ticks = [[0, 'In'], [1, 'Out']];
    if (options.flowHistogramOptions.mode.label === 'Stacked') {
        ticks = [[0, options?.flowHistogramOptions?.display?.label || 'Total']]
    }
    return {
        grid: {
            borderWidth: 0
        },
        series: {
            bars: {
                align: 'center',
                barWidth: 0.6,
                fill: 0.8,
                horizontal: options?.flowHistogramOptions?.direction?.label === 'Horizontal',
                lineWidth: 1,
                show: true,
            }
        },
        [axis]: {
            show: true,
            autoscaleMargin: 0.02,
            tickLength: 0,
            ticks,
        },
        legend: {
            show: options?.flowHistogramOptions.showLegend,
            position: options?.flowHistogramOptions?.position.value,
            margin: [-45, -45],
            backgroundColor: '#181b1f'
        },
    }
}
