import { SelectableValue } from '@grafana/data';
import { Select } from '@grafana/ui'
import { HelmColourThemeDisplay } from 'components/HelmColorThemeDisplay';
import { HelmInlineField } from 'components/HelmInlineField';
import React, { useState } from 'react'

interface AlarmTableAlarmDataState {
    styleWithSeverity?: SelectableValue<string | number>,
    severityTheme?: SelectableValue<string | number>
}

export const AlarmTableAlarms = () => {
    const [alarmTableAlarmData, setAlarmTableAlarmData] = useState<AlarmTableAlarmDataState>({});

    const setAlarmTableState = (key, value) => {
        const newState = { ...alarmTableAlarmData }
        newState[key] = value
        setAlarmTableAlarmData(newState);
    }

    return (
        <div>
            <HelmInlineField label="Style with severity">
                <Select
                    value={alarmTableAlarmData.styleWithSeverity}
                    onChange={(val) => setAlarmTableState('styleWithSeverity', val)}
                    options={[{ label: 'Row', value: 0 }, { label: 'Column', value: 1 }, { label: 'Off', value: 2 }]}
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
                    ]}
                />
            </HelmInlineField>
            <HelmColourThemeDisplay theme={alarmTableAlarmData.severityTheme?.value} />
        </div>
    )
}
