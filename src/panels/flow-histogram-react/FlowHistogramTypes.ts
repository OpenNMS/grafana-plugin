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
