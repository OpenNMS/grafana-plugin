import { SelectableValue } from '@grafana/data'

export interface AlarmTableControlState {
    indexes: boolean[];
    lastClicked: number;
}

export interface AlarmTableAlarmDataState {
    styleWithSeverity?: SelectableValue<string | number>,
    severityTheme?: SelectableValue<string | number>
}

export interface AlarmTableDataState {
    transformType?: SelectableValue<string | number>;
    columns: Array<SelectableValue<string | number>>
}

export interface AlarmTablePaginationState {
    rowsPerPage?: number;
    pauseRefresh: boolean;
    scroll: boolean;
    fontSize?: SelectableValue<string | number>
}

export interface AlarmTableControlProps {
    alarmTable: {
        alarmTableAlarms: AlarmTableAlarmDataState,
        alarmTableData: AlarmTableAlarmDataState,
        alarmTablePaging: AlarmTablePaginationState
    };
}

export interface AlarmTableControlActions {
    clear: () => void;
    escalate: () => void;
    acknowledge: () => void;
    details: () => void;
}
