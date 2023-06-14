import { useEffect, useState } from 'react'
import { DataFrame } from '@grafana/data'
import { ClientDelegate } from 'lib/client_delegate'
import { OnmsAlarm } from 'opennms/src/model'
import { getAlarmIdFromFields } from '../AlarmTableHelper'

export const useAlarm = (series: DataFrame[], soloIndex: number, client: ClientDelegate | undefined) => {
    const [alarmQuery, setAlarmQuery] = useState(false)
    const [alarmId, setAlarmId] = useState(-1)
    const [alarm, setAlarm] = useState<OnmsAlarm>()

    useEffect(() => {
        if (series?.[0]?.name === 'alarms') {
            setAlarmQuery(true)
            const localAlarmId = getAlarmIdFromFields(series?.[0].fields, soloIndex)
            setAlarmId(localAlarmId ?? -1)
        } else {
            setAlarmId(-1)
            setAlarmQuery(false)
        }

    }, [series, soloIndex])

    useEffect(() => {
        const updateAlarm = async () => {
            if (alarmId !== undefined && alarmId >= 0) {
              const returnedAlarm = await client?.getAlarm(alarmId)
              setAlarm(returnedAlarm)
            }
        }
        if (alarmId !== alarm?.id) {
            updateAlarm()
        }
    }, [alarm?.id, alarmId, client])

    const goToAlarm = () => {
        if (alarm?.detailsPage) {
            window.location.href = alarm.detailsPage
        }
    }

    return { alarm, alarmQuery, goToAlarm }
}
