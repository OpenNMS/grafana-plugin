import { PanelPlugin } from '@grafana/data';
import { AlarmTableControl } from './AlarmTableControl'
import { AlarmTableOptions } from './AlarmTableOptions';

export const plugin = new PanelPlugin(AlarmTableControl);
plugin.useFieldConfig()
plugin.setPanelOptions((builder) => {
    builder.addCustomEditor({ id: 'alarm-table-options', path: 'alarmTable', name: '', editor: AlarmTableOptions })
})
