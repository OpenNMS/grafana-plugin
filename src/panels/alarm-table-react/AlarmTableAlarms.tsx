import { Select } from '@grafana/ui'
import { HelmColorThemeDisplay } from 'components/HelmColorThemeDisplay';
import { HelmInlineField } from 'components/HelmInlineField';
import React, { useState, useEffect } from 'react'
import { AlarmTableAlarmDataState } from './AlarmTableTypes';


interface AlarmTableAlarmProps {
    onChange: Function
    alarmTable: any;
}
export const AlarmTableAlarms: React.FC<AlarmTableAlarmProps> = ({ onChange, alarmTable }) => {
    const [alarmTableAlarmData, setAlarmTableAlarmData] = useState<AlarmTableAlarmDataState>({
        styleWithSeverity: alarmTable.alarmTableAlarms?.styleWithSeverity || { label: 'Column', value: 1 },
        severityTheme: alarmTable.alarmTableAlarms?.severityTheme ||{ label: 'Helm Default', value: 0 }
    });

    useEffect(() => {
        onChange(alarmTableAlarmData)
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [alarmTableAlarmData])

    const setAlarmTableState = (key, value) => {
        const newState = { ...alarmTableAlarmData }
        newState[key] = value
        setAlarmTableAlarmData(newState);
    }

    return (
        <div>
            <HelmInlineField label="Style with severity">
                {/** 
                  * Support for row-level colours is not coded into the current version of Grafana: https://github.com/grafana/grafana/discussions/38151     
                  * If it's supported in the future, we can re-enable the option here.
                  *    */}
                <Select
                    value={alarmTableAlarmData.styleWithSeverity}
                    onChange={(val) => setAlarmTableState('styleWithSeverity', val)}
                    options={[{ label: 'Column', value: 1 }, { label: 'Off', value: 2 }]}
                />
            </HelmInlineField>
            <HelmInlineField label="Severity theme">
                <Select
                    value={alarmTableAlarmData.severityTheme}
                    onChange={(val) => setAlarmTableState('severityTheme', val)}
                    options={[
                        { label: 'Helm Default', value: 0 },
                        { label: 'OpenNMS', value: 1 },
                        { label: 'Oh My!', value: 2 },
                        { label: 'No, Never Mind (i)', value: 3 },
                        { label: 'That\'s Cool', value: 4 },
                        { label: 'Custom', value: 5 },
                    ]}
                />
            </HelmInlineField>
            <HelmColorThemeDisplay theme={alarmTableAlarmData.severityTheme?.value} />
        </div>
    )
}
