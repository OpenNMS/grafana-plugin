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

/** Build the default AlarmTableAdditionalState, enabling autoRefresh by default. */
const buildDefaultState = (state: AlarmTableAdditionalState | undefined) => {
  if (state) {
    return {
      ...state,
      autoRefresh: state.autoRefresh === undefined ? true : state.autoRefresh
    }
  }

  return { autoRefresh: true, useGrafanaUser: false }
}

export const AlarmTableAdditional: React.FC<AlarmTableAdditionalProps> = ({ onChange, context }) => {
    const [alarmTableAdditional, setAlarmTableAdditional] = useState<AlarmTableAdditionalState>(
      buildDefaultState(context?.options?.alarmTable?.alarmTableAdditional)
    )

    const userGrafanaUserTooltipText = 'Used to control whether operations on alarms are performed the data source user, ' +
      'or the user that is currently logged in to Grafana. ' +
      'Supported operations are escalating, clearing, un-acknowledging alarms ' +
      'as well as creating/updating a journal/memo. ' +
      'NOTE: The data source must be configured using an user with the \'admin\' role ' +
      'in order to perform actions as other users.'

    const autoRefreshTooltipText = 'Enables auto-refresh after performing an action such as acknowledge, clear or escalate. ' +
      'NOTE: This will refresh the entire dashboard.'

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
          <InlineField label='Use Grafana user' tooltip={userGrafanaUserTooltipText}>
            <div style={{ display: 'flex', alignItems: 'center', height: '32px' }}>
              <Switch
                value={alarmTableAdditional.useGrafanaUser}
                onChange={() => setAlarmTableState('useGrafanaUser', !alarmTableAdditional.useGrafanaUser)} />
            </div>
          </InlineField>
        </InlineFieldRow>
        <InlineFieldRow>
          <InlineField label='Auto refresh' tooltip={autoRefreshTooltipText}>
            <div style={{ display: 'flex', alignItems: 'center', height: '32px' }}>
              <Switch
                value={alarmTableAdditional.autoRefresh}
                onChange={() => setAlarmTableState('autoRefresh', !alarmTableAdditional.autoRefresh)} />
            </div>
          </InlineField>
        </InlineFieldRow>
      </div>
  )
}
