import { SelectableValue } from '@grafana/data'

export interface AlarmTableControlState {
  selectedAlarmIds: Set<number>
  selectedIndexes: boolean[]
  lastClicked: number
  lastClickedAlarmId: number
}

export interface AlarmTableAdditionalState {
  displayActionNotice: boolean
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

export interface AlarmTableOptionsState {
  alarmTableAdditional: AlarmTableAdditionalState
  alarmTableAlarms: AlarmTableAlarmDataState
  alarmTableData: AlarmTableDataState
  alarmTablePaging: AlarmTablePaginationState
  alarmTableColumnSizes?: AlarmTableColumnSizeState
}

export interface AlarmTableControlProps {
  alarmTable: AlarmTableOptionsState
}

export interface AlarmTableControlActions {
  clear: () => void
  escalate: () => void
  acknowledge: () => void
  details: () => void
}
