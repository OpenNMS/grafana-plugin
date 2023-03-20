import React from 'react'
import { OnmsAlarm } from 'opennms/src/model'

interface AlarmTableModalJsonProps {
    alarm: OnmsAlarm | undefined;
}

export const AlarmTableModalJson: React.FC<AlarmTableModalJsonProps> = ({alarm}) => {
    return (<div>
        <pre>
            {JSON.stringify(alarm,null,2)}
        </pre>
    </div>)
}
