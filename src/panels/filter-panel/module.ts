import { PanelPlugin } from '@grafana/data';
import { FilterPanelControl } from './FilterPanelControl'
import { FilterPanelOptions } from './FilterPanelOptions'

export const plugin = new PanelPlugin(FilterPanelControl).setPanelOptions((builder) => {
    builder.addCustomEditor({ id: 'filter-editor', path: 'filterEditor', name: 'Filter', editor: FilterPanelOptions })
});
