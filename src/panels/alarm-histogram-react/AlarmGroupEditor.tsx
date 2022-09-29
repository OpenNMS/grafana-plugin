import React from 'react'
import { Select } from '@grafana/ui'
import { AlarmGroups } from './constants'
import { PanelOptionsEditorProps } from '@grafana/data'

interface Props extends PanelOptionsEditorProps<number> { }

export const AlarmGroupEditor: React.FC<Props> = ({ value, onChange }) => {
    return (
        <div>
            <Select options={[
                { ...AlarmGroups.Acknowledged },
                { ...AlarmGroups.Severity },
            ]}
                value={value}
                onChange={(e) => onChange(e.value)}
            />
        </div>
    )
}
