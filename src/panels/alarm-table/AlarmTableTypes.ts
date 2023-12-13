import { SelectableValue } from '@grafana/data'

export interface AlarmTableControlState {
  indexes: boolean[]
  lastClicked: number
}

export interface AlarmTableAdditionalState {
  autoRefresh: boolean
  useGrafanaUser: boolean
}

export interface AlarmTableAlarmDataState {
  styleWithSeverity?: SelectableValue<string | number>
  severityTheme?: SelectableValue<string | number>
}

export interface AlarmTableDataState {
  transformType?: SelectableValue<string | number>
  columns: Array<SelectableValue<string | number>>
}

export interface AlarmTablePaginationState {
  rowsPerPage?: number
  pauseRefresh: boolean
  scroll: boolean
  fontSize?: SelectableValue<string | number>
}

export interface AlarmTableColumnSizeItem {
  fieldName: string
  width: number
}

export interface AlarmTableColumnSizeState {
  active: boolean
  columnSizes: AlarmTableColumnSizeItem[]
}

export interface AlarmTableControlProps {
  alarmTable: {
    alarmTableAdditional: AlarmTableAdditionalState
    alarmTableAlarms: AlarmTableAlarmDataState
    alarmTableData: AlarmTableAlarmDataState
    alarmTablePaging: AlarmTablePaginationState
    alarmTableColumnSizes?: AlarmTableColumnSizeState
  }
}

export interface AlarmTableControlActions {
  clear: () => void
  escalate: () => void
  acknowledge: () => void
  details: () => void
}
