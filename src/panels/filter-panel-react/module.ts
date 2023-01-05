import { PanelPlugin } from '@grafana/data';
import { FilterControl } from './FilterPanelControl'
import { FilterEditor } from './FilterPanelOptions'

export const plugin = new PanelPlugin(FilterControl).setPanelOptions((builder) => {
    builder.addCustomEditor({ id: 'filter-editor', path: 'filterEditor', name: 'Filter', editor: FilterEditor })
});
