import React from 'react'
import { OnmsAlarm } from 'opennms/src/model'

interface AlarmTableModalOverviewProps {
    alarm: OnmsAlarm | undefined
}

export const AlarmTableModalOverview: React.FC<AlarmTableModalOverviewProps> = ({ alarm }) => {
    return (<div>
        <style>
            {
                `
                .alarm-details-table tr{
                        height:32px;
                        padding-left:12px;
                }
                .alarm-details-table tr:nth-child(2n+1) {
                    background-color:#1c1f23;
                }
                .alarm-details-table th {
                    padding-left:12px;
                    padding-right:12px;
                    white-space: nowrap;
                }
                .alarm-details-table td {
                    padding-top:12px;
                    padding-bottom:12px;
                }
                `
            }
        </style>
        <table className='alarm-details-table'>
            <tr>
                <th>Alarm ID</th>
                <td>{alarm?.id}</td>
            </tr>
            <tr>
                <th>UEI</th>
                <td className="wrap">{alarm?.uei}</td>
            </tr>
            <tr>
                <th>Severity</th>
                <td>{alarm?.severity?.label}</td>
            </tr>
            <tr>
                <th>Last Event Time</th>
                <td>{alarm?.lastEvent?.time?.toString()}</td>
            </tr>
            <tr>
                <th>First Event Time</th>
                <td>{alarm?.firstEventTime?.toString()}</td>
            </tr>
            <tr>
                <th>Log Message</th>
                <td dangerouslySetInnerHTML={{__html:alarm?.logMessage || ''}}></td>
            </tr>
            <tr>
                <th>Description</th>
                <td dangerouslySetInnerHTML={{__html:alarm?.description || ''}}></td>
            </tr>
            <tr>
                <th>Count</th>
                <td>{alarm?.count}</td>
            </tr>
            <tr>
                <th>Reduction Key</th>
                <td>{alarm?.reductionKey}</td>
            </tr>
            {alarm?.managedObjectInstance && <tr>
                <th>Managed Object Instance</th>
                <td>{alarm?.managedObjectInstance}</td>
            </tr>}
            {alarm?.managedObjectType && <tr>
                <th>Managed Object Type</th>
                <td>{alarm?.managedObjectType}</td>
            </tr>}

        </table>
    </div>)
}
