import { OnmsAlarm } from 'opennms/src/model'
import React from 'react'

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
