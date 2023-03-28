import { useState, useEffect } from 'react'
import { Field } from '@grafana/data'
import { getBackendSrv } from '@grafana/runtime'
import { ClientDelegate } from 'lib/client_delegate'
import { getAlarmIdFromFields } from '../AlarmTableHelper'

export const useAlarmTableMenuActions = (indexes, fields: Field[], closeMenu, client: ClientDelegate | undefined) => {
    const [detailsModal, setDetailsModal] = useState(false)
    const [user, setUser] = useState<{ id: string }>();

    useEffect(() => {
        const getUserFromGrafana = async () => {
            setUser((await getBackendSrv().get('/api/users'))?.[0])
        }

        getUserFromGrafana();
    }, [])

    const loopAction = async (action) => {
        for (let i = 0; i < indexes.length; i++) {
            if (indexes[i]) {
                const alarmId = getAlarmIdFromFields(fields, i);
                await action(alarmId, user?.id);
            }
        }
    }

    const clear = async () => {
        await loopAction(async (alarmId, userId) => {
            await client?.doClear(alarmId, user?.id)
        })
        closeMenu();
    }

    const details = () => {
        closeMenu();
        setDetailsModal(true);
    }

    const escalate = async () => {
        await loopAction(async (alarmId, userId) => {
            await client?.doEscalate(alarmId, user?.id)
        })
        closeMenu();
    }

    const acknowledge = async () => {
        await loopAction(async (alarmId, userId) => {
            await client?.doAck(alarmId, user?.id)
        })
        closeMenu();
    }

    return { actions: { clear, details, escalate, acknowledge }, detailsModal, setDetailsModal }
}
