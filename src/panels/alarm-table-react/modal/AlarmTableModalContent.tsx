import { ClientDelegate } from 'lib/client_delegate'
import { OnmsAlarm } from 'opennms/src/model'
import React, { useEffect, useState } from 'react'
import { AlarmTableModalJson } from './AlarmTableModalJson'
import { AlarmTableModalMemos } from './AlarmTableModalMemos'
import { AlarmTableModalOverview } from './AlarmTableModalOverview'

interface AlarmTableModalContentProps {
    tab: number,
    alarmId: string,
    client: ClientDelegate | undefined;
}

export const AlarmTableModalContent: React.FC<AlarmTableModalContentProps> = ({ tab, alarmId, client }) => {
   
    return (
        <>
            {tab === 0 && <AlarmTableModalOverview alarm={alarm} />}
            {tab === 1 && <AlarmTableModalMemos alarm={alarm} client={client} />}
            {tab === 2 && <AlarmTableModalJson alarm={alarm} />}
        </>
    )
}
