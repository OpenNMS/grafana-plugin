import React, { useState, useEffect } from 'react'
import { Input, Switch, Select } from '@grafana/ui'
import { OnmsInlineField } from 'components/OnmsInlineField'
import { SwitchBox } from 'components/SwitchBox'
import { AlarmTablePaginationState } from './AlarmTableTypes'

interface AlarmTablePagingPanelProps {
    onChange: Function
    context: any
}

export const AlarmTablePaging: React.FC<AlarmTablePagingPanelProps> = ({ onChange, context }) => {

    const [alarmTablePaging, setAlarmTablePaging] = useState<AlarmTablePaginationState>(context?.options?.alarmTable?.alarmTablePaging || {
        pauseRefresh: false,
        scroll: true,
        rowsPerPage: 10,
        fontSize: { label: '100%', value: 2 }
    });

    const setAlarmTablePagingState = (key, value) => {
        const newState = { ...alarmTablePaging }
        newState[key] = value
        setAlarmTablePaging(newState);
    }

    useEffect(() => {
        onChange(alarmTablePaging)
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [alarmTablePaging])

    const fontSizeOptions = [
        { label: '80%', value: 0 },
        { label: '90%', value: 1 },
        { label: '100%', value: 2 },
        { label: '110%', value: 3 },
        { label: '120%', value: 4 },
        { label: '130%', value: 5 },
        { label: '140%', value: 6 },
        { label: '150%', value: 7 },
        { label: '160%', value: 8 },
        { label: '180%', value: 9 },
        { label: '200%', value: 10 },
        { label: '220%', value: 11 },
        { label: '250%', value: 12 },
    ]

    return (
        <div>
            <OnmsInlineField label='Rows per page'>
                <Input type='number' value={alarmTablePaging.rowsPerPage} onChange={(val) => setAlarmTablePagingState('rowsPerPage', val.currentTarget.value)} />
            </OnmsInlineField>
            {/**   
              * 
              * This is in the original Alarm Table Panel, but I don't see any use for it currently and it might be a holdover from older functionality.
              * TODO: Determine if we should keep this or not.
              *  <OnmsInlineField label='Pause refresh'>
                <SwitchBox>
                    <Switch value={alarmTablePaging.pauseRefresh} onChange={(val) => setAlarmTablePagingState('pauseRefresh', !alarmTablePaging.pauseRefresh)} />
                </SwitchBox>
            </OnmsInlineField> */}
            <OnmsInlineField label='Scroll'>
                <SwitchBox>
                    <Switch value={alarmTablePaging.scroll} onChange={(val) => setAlarmTablePagingState('scroll', !alarmTablePaging.scroll)} />
                </SwitchBox>
            </OnmsInlineField>
            <OnmsInlineField label='Font size'>
                <Select
                    value={alarmTablePaging.fontSize}
                    options={fontSizeOptions}
                    onChange={(val) => setAlarmTablePagingState('fontSize', val)}
                />
            </OnmsInlineField>
        </div>
    )
}
