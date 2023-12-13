import { useState, useEffect } from 'react'
import { Field } from '@grafana/data'
import { getBackendSrv } from '@grafana/runtime'
import { ClientDelegate } from 'lib/client_delegate'
import { getAlarmIdFromFields } from '../AlarmTableHelper'

/**
 * Hooks for the Alarm Table Panel action menu.
 *
 * @param indexes Selection state of alarms in the table (0-based, by row).
 * @param fields Alarm Field data, from DataFrame API response.
 * @param closeMenu Function from the calling component to close the action menu.
 * @param actionCallback A callback to perform after an action; currently used to refresh the alarm panel (actually, the dashboard)
 *   after an 'acknowledge', 'clear' or 'escalate'. Pass 'null' to disable.
 * @param useGrafanaUser If true, use the current Grafana user for the API call; this username will be saved in the 
 *   OpenNMS database as the user who performed the action.
 *   Otherwise uses the user configured in the Entity datasource.
 * @param client ClientDelegate for API calls.
 * @returns 
 */
export const useAlarmTableMenuActions = (
  indexes: boolean[],
  fields: Field[],
  closeMenu: () => void,
  actionCallback: (actionName: string) => void | null,
  useGrafanaUser: boolean,
  client: ClientDelegate | undefined) => {

  const [detailsModal, setDetailsModal] = useState(false)
  // 'id' is the Grafana user id, this doesn't correlate to anything in OpenNMS
  // 'login' should match OpenNMS username
  const [user, setUser] = useState<{ id: string, login: string }>()

  useEffect(() => {
    const getUserFromGrafana = async () => {
      if (useGrafanaUser) {
        // Grafana 'Get Actual User' API to get current user
        const apiUser = await getBackendSrv().get('/api/user')
        setUser(apiUser)
      } else {
        // unset user to use the user configured in the Entity datasource  
        setUser(undefined)
      }
    }

    getUserFromGrafana()
  }, [useGrafanaUser])

  const loopAction = async (action) => {
    for (let i = 0; i < indexes.length; i++) {
      if (indexes[i]) {
        const alarmId = getAlarmIdFromFields(fields, i)
        await action(alarmId, user?.login)
      }
    }
  }

  const clear = async () => {
    await loopAction(async (alarmId, userId) => {
      await client?.doClear(alarmId, user?.login)
    })

    if (actionCallback) {
      actionCallback('clear')
    }

    closeMenu()
  }

  const details = () => {
    closeMenu()
    setDetailsModal(true)
  }

  const escalate = async () => {
    await loopAction(async (alarmId, userId) => {
      await client?.doEscalate(alarmId, user?.login)
    })
    
    if (actionCallback) {
      actionCallback('escalate')
    }

    closeMenu()
  }

  const acknowledge = async () => {
    await loopAction(async (alarmId, userId) => {
      await client?.doAck(alarmId, user?.login)
    })

    if (actionCallback) {
      actionCallback('acknowledge')
    }

    closeMenu()
  }

  return { actions: { clear, details, escalate, acknowledge }, detailsModal, setDetailsModal }
}
