import { PanelPlugin } from '@grafana/data';
import { AlarmTableAlarms } from './AlarmTableAlarms';
import { AlarmTableColumnStyles } from './AlarmTableColumnStyles';
import { AlarmTableControl } from './AlarmTableControl'
import { AlarmTableData } from './AlarmTableData';
import { AlarmTablePaging } from './AlarmTablePaging';

export const plugin = new PanelPlugin(AlarmTableControl).setPanelOptions((builder) => {
    builder.addCustomEditor({id: 'alarm-table-data',path: 'alarmTableData', name:'Data',editor: AlarmTableData})
    builder.addCustomEditor({id: 'alarm-table-paging',path: 'alarmTablePaging', name:'Paging',editor: AlarmTablePaging })
    builder.addCustomEditor({id: 'alarm-table-alarms',path: 'alarmTableAlarms', name:'Alarms',editor: AlarmTableAlarms})
    builder.addCustomEditor({id: 'alarm-table-column-styles',path: 'alarmTableColumnStyles', name:'ColumnStyles',editor: AlarmTableColumnStyles})
});
