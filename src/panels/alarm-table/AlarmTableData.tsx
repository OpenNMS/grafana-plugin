import { Select } from '@grafana/ui'
import { HelmDragList } from 'components/HelmDragList';
import { HelmInlineField } from 'components/HelmInlineField';
import React, { useState, useEffect } from 'react'
import { AlarmTableDataState } from './AlarmTableTypes';



interface AlarmTableDataProps {
    onChange: Function;
    context: any;
}

export const AlarmTableData: React.FC<AlarmTableDataProps> = ({ onChange, context }) => {
    const [alarmTableData, setAlarmTableData] = useState<AlarmTableDataState>(context?.options?.alarmTable?.alarmTableData || {
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
        onChange(alarmTableData);
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [alarmTableData])
    const setAlarmTableState = (key, value) => {
        const newState = { ...alarmTableData }
        newState[key] = value
        setAlarmTableData(newState);
    }
    return (
        <div>

            {/** 
              * 
              * Commented out for now. This is in the original table, but 
              * from what I can tell there was only ever one transformer
              * written, so there's no need to have the ability to swap them
              * TODO: Double check if we need the ability to 'transform' in different ways
              * 
              * <HelmInlineField label="Table Transform">
                <Select
                    value={alarmTableData.transformType}
                    onChange={(val) => setAlarmTableState('transformType', val)}
                    options={[{ label: 'Table', value: 0 }]}
                />
            </HelmInlineField> */}

            <HelmInlineField label="Columns">

                <Select
                    placeholder='Add Column'
                    value={''}
                    onChange={(val) => {
                        const newColumns = [...alarmTableData.columns]
                        newColumns.push(val)
                        setAlarmTableState('columns', newColumns)
                    }}
                    options={context?.data?.[0]?.fields.map((field, index) => ({ ...field, value: index, label: field.name }))}
                />


            </HelmInlineField>
            <HelmDragList values={alarmTableData?.columns} onChange={(val) => { setAlarmTableState('columns', val) }} />

        </div>
    )
}
