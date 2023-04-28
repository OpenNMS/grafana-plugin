import React, { useEffect, useState } from 'react'
import {
  InlineField,
  InlineFieldRow,
  Switch
} from '@grafana/ui'
import { AlarmTableAdditionalState } from './AlarmTableTypes'

interface AlarmTableAdditionalProps {
    onChange: Function;
    context: any;
}

export const AlarmTableAdditional: React.FC<AlarmTableAdditionalProps> = ({ onChange, context }) => {
    const [alarmTableAdditional, setAlarmTableAdditional] = useState<AlarmTableAdditionalState>(context?.options?.alarmTable?.alarmTableAdditional || 
      { useGrafanaUser: false })
    const tooltipText = 'Used to control whether operations on alarms are performed the data source user, ' +
      'or the user that is currently logged in to Grafana. ' +
      'Supported operations are escalating, clearing, un-acknowledging alarms ' +
      'as well as creating/updating a journal/memo. ' +
      'NOTE: The data source must be configured using an user with the \'admin\' role ' +
      'in order to perform actions as other users.'

    useEffect(() => {
        onChange(alarmTableAdditional);
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [alarmTableAdditional])

    const setAlarmTableState = (key, value) => {
        const newState = { ...alarmTableAdditional }
        newState[key] = value
        setAlarmTableAdditional(newState)
   }

    return (
      <div>
        <InlineFieldRow>
          <InlineField label='Use Grafana user' tooltip={tooltipText}>
            <div style={{ display: 'flex', alignItems: 'center', height: '32px' }}>
              <Switch
                value={alarmTableAdditional.useGrafanaUser}
                onChange={() => setAlarmTableState('useGrafanaUser', !alarmTableAdditional.useGrafanaUser)} />
            </div>
          </InlineField>
        </InlineFieldRow>
      </div>
  )
}
