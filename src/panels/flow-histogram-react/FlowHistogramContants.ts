import { FlowHistogramOptionsProps, FlowPanelUnitInfo } from "./FlowHistogramTypes";
import { validateFlowHistogramPanelData } from "./FlowHistogramHelpers"
import { DataFrame } from "@grafana/data"

export const DirectionOptions = [
    { label: 'Horizontal', value: '0' },
    { label: 'Vertical', value: '1' }
]
export const UnitOptions = [
    { label: 'B', value: '0' },
    { label: 'KB', value: '1' },
    { label: 'MB', value: '2' },
    { label: 'GB', value: '3' },
]

export const DisplayOptions = [
    { label: 'Total', value: '0' },
    { label: 'Rate', value: '1' },
]

export const ModeOptions = [
    { label: 'Separate', value: '0' },
    { label: 'Stacked', value: '1' },
]

export const PositionOptions = [
    { label: 'Right Side', value: '' },
    { label: 'Under Graph', value: '' },
]

export const UnitInfo = (options: { flowHistogramOptions: FlowHistogramOptionsProps }, dataSeries: DataFrame[]): FlowPanelUnitInfo => {
    let divisor = 1
    let units = 'Bytes'
    
    if(!validateFlowHistogramPanelData(dataSeries)){
        return { units, divisor }
    }
    
    const toBits = dataSeries?.[0]?.meta?.custom?.['toBits'] ? true : false
    const rate = options.flowHistogramOptions.display.label === 'Rate'
    const option = options.flowHistogramOptions.units.label

    
    switch (option) {
        case 'B':
            units = toBits ? 'Bits' : 'Bytes'
            break
        case 'KB':
            divisor = 1024
            units = toBits ? 'Kb' : 'KB'
            break
        case 'MB':
            divisor = 1024 ** 2
            units = toBits ? 'Mb' : 'MB'
            break
        case 'GB':
            divisor = 1024 ** 3
            units = toBits ? 'Gb' : 'GB'
            break
    }
    if (rate) {
        units = units + '/s'
    }
    return { units, divisor }

}

export const DataPosition = {
    horizontal: {
        value: 0,
        index: 0,
        metric: 1
    },
    vertical: {
        value: 1,
        index: 1,
        metric: 0
    },
    indexLabel: {
        index: 0,
        label: 1
    }
}

export const FLowDataDirection = {
    dataIn: { label: 'In', value: 0 },
    dataOut: { label: 'Out', value: 1 }
}
