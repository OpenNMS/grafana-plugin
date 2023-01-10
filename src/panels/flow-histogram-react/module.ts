import { PanelPlugin } from '@grafana/data';
import { FlowHistogramControl } from './FlowHistogramControl'
import { FlowHistogramOptions } from './FlowHistogramOptions'

export const plugin = new PanelPlugin(FlowHistogramControl).setPanelOptions((builder) => {
    builder.addCustomEditor({ id: 'flow-histogram-options', path: 'flowHistogramOptions', name: 'Flow Histogram Panel', editor: FlowHistogramOptions })
});
