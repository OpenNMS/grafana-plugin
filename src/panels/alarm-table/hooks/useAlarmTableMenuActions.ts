import { useState, useEffect } from 'react'
import { Field } from '@grafana/data'
import { getBackendSrv } from '@grafana/runtime'
import { ClientDelegate } from 'lib/client_delegate'
import { getAlarmIdFromFields } from '../AlarmTableHelper'

export const useAlarmTableMenuActions = (indexes: boolean[], fields: Field[],
  closeMenu, useGrafanaUser: boolean, client: ClientDelegate | undefined) => {
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
    closeMenu()
  }

  const acknowledge = async () => {
    await loopAction(async (alarmId, userId) => {
      await client?.doAck(alarmId, user?.login)
    })
    closeMenu()
  }

  return { actions: { clear, details, escalate, acknowledge }, detailsModal, setDetailsModal }
}
