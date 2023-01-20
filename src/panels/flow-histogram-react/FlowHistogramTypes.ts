import { SelectableValue } from "@grafana/data"

export interface FlowHistogramOptionsProps {
    display: SelectableValue<string>,
    direction: SelectableValue<string>,
    units: SelectableValue<string>
    mode: SelectableValue<string>
    showLegend: boolean
    position: SelectableValue<string>,
    height: number
}

export interface DataProcessed{
    ticks?: any[],
}

export interface SeparateDataProcessed extends DataProcessed{
    inByLabel: any[],
    outByLabel: any[],    
}

export interface StackedDataProcessed extends DataProcessed{
    metricsByLabel: any[]
}

export interface FlowPanelDataProcessed {    
    separateData?: SeparateDataProcessed 
    stackedData?: StackedDataProcessed
}

export interface FlowPanelUnitInfo{
    units: string,
    divisor: number
}
