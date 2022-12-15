import { useEffect, useState } from 'react'
import { DataFrame } from "@grafana/data"
import { ClientDelegate } from 'lib/client_delegate';
import { OnmsAlarm } from 'opennms/src/model';

export const useAlarm = (series: DataFrame[], soloIndex: number, client: ClientDelegate | undefined) => {
    const [alarmQuery, setAlarmQuery] = useState(false);
    const [alarmId, setAlarmId] = useState(-1);
    const [alarm, setAlarm] = useState<OnmsAlarm>();

    useEffect(() => {
        if (series?.[0].name === 'alarms') {
            setAlarmQuery(true)
            const localAlarmId = series?.[0].fields.find((d) => d.name === 'ID')?.values.get(soloIndex)
            setAlarmId(localAlarmId)
        } else {
            setAlarmId(-1)
            setAlarmQuery(false)
        }

    }, [series, soloIndex])

    useEffect(() => {
        const updateAlarm = async () => {
            const returnedAlarm = await client?.getAlarm(alarmId);
            setAlarm(returnedAlarm)
        }
        if (alarmId !== alarm?.id) {
            updateAlarm();
        }
    }, [alarm?.id, alarmId, client])

    const goToAlarm = () => {
        if (alarm?.detailsPage) {
            window.location.href = alarm.detailsPage
        }
    }

    return { alarm, alarmId, alarmQuery, goToAlarm }
}
