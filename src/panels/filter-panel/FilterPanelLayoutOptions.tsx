import React from 'react'
import { Switch } from '@grafana/ui'
import { OnmsInlineField } from 'components/OnmsInlineField'
import { SwitchBox } from 'components/SwitchBox'

interface FilterPanelLayoutOptionsProps {
    isHorizontal?: boolean
    onChange: Function
}

export const FilterPanelLayoutOptions: React.FC<FilterPanelLayoutOptionsProps> = ({ isHorizontal, onChange }) => {
    return (
      <OnmsInlineField label='Use horizontal layout'>
        <SwitchBox>
          <Switch
            value={isHorizontal}
            onChange={(e) => onChange(!isHorizontal)} />
        </SwitchBox>
      </OnmsInlineField>
    )
}
