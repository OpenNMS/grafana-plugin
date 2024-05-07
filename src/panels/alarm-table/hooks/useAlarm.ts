import { MutableRefObject, useEffect, useState } from 'react'
import { DataFrame } from '@grafana/data'
import { ClientDelegate } from 'lib/client_delegate'
import { OnmsAlarm } from 'opennms/src/model'

export const useAlarm = (frame: MutableRefObject<DataFrame>, soloAlarmId: number, client: ClientDelegate | undefined) => {
  const [alarmQuery, setAlarmQuery] = useState(false)
  const [alarmId, setAlarmId] = useState(-1)
  const [alarm, setAlarm] = useState<OnmsAlarm>()

  useEffect(() => {
    if (frame.current.name === 'alarms') {
      setAlarmQuery(true)
      setAlarmId(soloAlarmId ?? -1)
    } else {
      setAlarmId(-1)
      setAlarmQuery(false)
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [frame.current, soloAlarmId])

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
