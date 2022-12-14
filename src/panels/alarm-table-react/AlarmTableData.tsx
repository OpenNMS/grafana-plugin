import { PanelOptionsEditorProps } from '@grafana/data';
import { Select } from '@grafana/ui'
import { HelmDragList } from 'components/HelmDragList';
import { HelmInlineField } from 'components/HelmInlineField';
import React, { useState, useEffect } from 'react'
import { AlarmTableDataState } from './AlarmTableTypes';



interface AlarmTableDataProps {

}

export const AlarmTableData: React.FC<PanelOptionsEditorProps<AlarmTableDataProps>> = (props) => {
    const [alarmTableData, setAlarmTableData] = useState<AlarmTableDataState>({
        columns: [
            { label: 'Is Acknowledged', value: 20 },
            { label: 'Severity', value: 5 },
            { label: 'Count', value: 1 },
            { label: 'Last Event Time', value: 23 },
            { label: 'Location', value: 8 },
            { label: 'Node Label', value: 14 },
            { label: 'Log Message', value: 9 },
        ]
    });
    useEffect(() => {
        setAlarmTableState('columns', alarmTableData.columns)
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])
    useEffect(() => {
        props.onChange(alarmTableData);
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [alarmTableData])
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

                <Select
                    placeholder='Add Column'
                    value={0}
                    onChange={(val) => {
                        const newColumns = [...alarmTableData.columns]
                        newColumns.push(val)
                        setAlarmTableState('columns', newColumns)
                    }}
                    options={props?.context?.data?.[0]?.fields.map((field, index) => ({ ...field, value: index, label: field.name }))}
                />


            </HelmInlineField>
            <HelmDragList values={alarmTableData?.columns} onChange={(val) => { setAlarmTableState('columns', val) }} />

        </div>
    )
}
