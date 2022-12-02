import { SelectableValue } from '@grafana/data';
import { Select, MultiSelect } from '@grafana/ui'
import { HelmInlineField } from 'components/HelmInlineField';
import React, { useState } from 'react'

interface AlarmTableDataState {
    transformType?: SelectableValue<string | number>;
    columns: Array<SelectableValue<string | number>>
}

export const AlarmTableData = () => {
    const [alarmTableData, setAlarmTableData] = useState<AlarmTableDataState>({ columns: [] });

    const setAlarmTableState = (key, value) => {
        const newState = { ...alarmTableData }
        newState[key] = value
        setAlarmTableData(newState);
    }

    return (
        <div>
          
            <HelmInlineField label="Table Transform">
                <Select
                    value={alarmTableData.transformType}
                    onChange={(val) => setAlarmTableState('transformType', val)}
                    options={[{ label: 'Table', value: 0 }]}
                />
            </HelmInlineField>

            <HelmInlineField label="Columns">
                <MultiSelect
                    value={alarmTableData.columns}
                    onChange={(val) => {
                        setAlarmTableState('columns', val)
                    }}
                    options={[{ label: 'Is Acknowledged', value: 0 }, { label: 'Severity', value: 1 }]}
                />
            </HelmInlineField>
        </div>
    )
}
