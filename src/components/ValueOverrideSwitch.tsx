import React from 'react'
import { InlineField, SegmentInput, Switch } from '@grafana/ui'

export interface ValueOverrideSwitchProps {
  override: boolean
  setOverride: (value: boolean) => void
  value: string
  setValue: (value: any) => void
  label?: string
  placeholder?: string
  tooltip?: string
}

export const ValueOverrideSwitch = (props: ValueOverrideSwitchProps) => {
  const defaultLabel = 'Enter value:'
  const defaultPlaceholder = 'Enter variable or value'
  const defaultTooltip = 'Enter a value or template variable manually'

  return (
    <>
    <style>
      {`
        .value-override-switch-field .value-override-switch-wrapper {
            display: flex;
            align-items: center;
            height: 32px;
            width: 32px;
        }
        .value-override-switch-field .value-override-switch-wrapper label {
            min-width: 32px;
            width: 32px;
        }
      `}
    </style>
    <InlineField className='value-override-switch-field' label={props.label || defaultLabel} tooltip={props.tooltip || defaultTooltip}>
      <div className='value-override-switch-wrapper'>
        <Switch
          value={props.override}
          onChange={() => props.setOverride(!props.override)} />
      </div>
    </InlineField>
    {
      (props.override || !!props.value?.length) &&
      <SegmentInput
        disabled={!props.override}
        value={props.value}
        placeholder={props.placeholder || defaultPlaceholder}
        onChange={(value) => props.setValue(value.toString())}
      />
    }
    </>
  )
}
