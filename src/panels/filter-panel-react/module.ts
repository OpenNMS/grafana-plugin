import { PanelPlugin } from '@grafana/data';
import { FilterControl } from './FilterControl'
import { FilterEditor } from './FilterEditor'

export const plugin = new PanelPlugin(FilterControl).setPanelOptions((builder) => {
    builder.addCustomEditor({ id: 'filter-editor', path: 'filterEditor', name: 'Filter', editor: FilterEditor })
});
