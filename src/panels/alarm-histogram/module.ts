import { PanelPlugin } from '@grafana/data'
import { AlarmHistogramControl } from './AlarmHistogramControl'
import { AlarmGroupEditor, AlarmDirectionEditor } from '.'

export const plugin = new PanelPlugin(AlarmHistogramControl).setPanelOptions((builder) => {
    builder.addCustomEditor({id: 'alarm-group',path: 'alarmGroup', name:'Alarm Group',editor: AlarmGroupEditor})
    builder.addCustomEditor({id: 'alarm-direction',path: 'alarmDirection', name:'Alarm Direction',editor: AlarmDirectionEditor})
})
