import { PanelPlugin } from '@grafana/data';
import { AlarmTableAlarms } from './AlarmTableAlarms';
import { AlarmTableControl } from './AlarmTableControl'
import { AlarmTableData } from './AlarmTableData';
import { AlarmTablePaging } from './AlarmTablePaging';

export const plugin = new PanelPlugin(AlarmTableControl);
plugin.useFieldConfig()
plugin.setPanelOptions((builder) => {
    builder.addCustomEditor({ id: 'alarm-table-data', path: 'alarmTableData', name: 'Data', editor: AlarmTableData })
    builder.addCustomEditor({ id: 'alarm-table-paging', path: 'alarmTablePaging', name: 'Paging', editor: AlarmTablePaging })
    builder.addCustomEditor({ id: 'alarm-table-alarms', path: 'alarmTableAlarms', name: 'Alarms', editor: AlarmTableAlarms })
})
