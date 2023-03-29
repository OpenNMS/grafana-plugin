import React from 'react'
import { ClientDelegate } from 'lib/client_delegate'
import { OnmsAlarm } from 'opennms/src/model'
import { AlarmTableModalJson } from './AlarmTableModalJson'
import { AlarmTableModalMemos } from './AlarmTableModalMemos'
import { AlarmTableModalOverview } from './AlarmTableModalOverview'

interface AlarmTableModalContentProps {
    tab: number;
    alarm: OnmsAlarm | undefined;
    client: ClientDelegate | undefined;
}

export const AlarmTableModalContent: React.FC<AlarmTableModalContentProps> = ({ tab, alarm, client }) => {
    return (
        <>
            {tab === 0 && <AlarmTableModalOverview alarm={alarm} />}
            {tab === 1 && <AlarmTableModalMemos alarm={alarm} client={client} />}
            {tab === 2 && <AlarmTableModalJson alarm={alarm} />}
        </>
    )
}
