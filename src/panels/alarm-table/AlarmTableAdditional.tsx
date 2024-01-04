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

/** Build the default AlarmTableAdditionalState, enabling displayActionNotice by default. */
const buildDefaultState = (state: AlarmTableAdditionalState | undefined) => {
  if (state) {
    return {
      ...state,
      displayActionNotice: state.displayActionNotice === undefined ? true : state.displayActionNotice
    }
  }

  return { displayActionNotice: true, useGrafanaUser: false }
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

    const actionNoticeTooltipText = 'Enables a notice to be displayed after performing an action such as acknowledge, clear or escalate ' +
      'which will notify the user whether the action succeeded and prompt them to refresh the panel.'

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
          <InlineField label='Display action notice' tooltip={actionNoticeTooltipText}>
            <div style={{ display: 'flex', alignItems: 'center', height: '32px' }}>
              <Switch
                value={alarmTableAdditional.displayActionNotice}
                onChange={() => setAlarmTableState('displayActionNotice', !alarmTableAdditional.displayActionNotice)} />
            </div>
          </InlineField>
        </InlineFieldRow>
      </div>
  )
}
