import React from 'react'
import { PanelOptionsEditorProps } from '@grafana/data'
import { Select } from '@grafana/ui'
import { AlarmDirections } from './constants'

interface Props extends PanelOptionsEditorProps<number> { }

export const AlarmDirectionEditor: React.FC<Props> = ({ value, onChange }) => {
    return (
        
        <div>
            <Select options={[
                { ...AlarmDirections.Vertical },
                { ...AlarmDirections.Horizontal },
            ]}

                value={value}
                onChange={(e) => onChange(e.value)}
            />
        </div>
    )
}
