import { SelectableValue } from '@grafana/data';
import { Input, Switch, Select } from '@grafana/ui'
import { HelmInlineField } from 'components/HelmInlineField';
import { SwitchBox } from 'components/SwitchBox';
import React, { useState } from 'react'

interface AlarmTablePagingState {
    rowsPerPage?: number;
    pauseRefresh: boolean;
    scroll: boolean;
    fontSize?: SelectableValue<string | number>
}
export const AlarmTablePaging = () => {

    const [alarmTablePaging, setAlarmTablePaging] = useState<AlarmTablePagingState>({pauseRefresh:false,scroll:false});

    const setAlarmTablePagingState = (key, value) => {
        const newState = { ...alarmTablePaging }
        newState[key] = value
        setAlarmTablePaging(newState);
    }

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
            <HelmInlineField label='Rows per page'>
                <Input type='number' value={alarmTablePaging.rowsPerPage} onChange={(val) => setAlarmTablePagingState('rowsPerPage', val.currentTarget.value)} />
            </HelmInlineField>
            <HelmInlineField label='Pause refresh'>
                <SwitchBox>
                    <Switch value={alarmTablePaging.pauseRefresh} onChange={(val) => setAlarmTablePagingState('pauseRefresh', !alarmTablePaging.pauseRefresh)} />
                </SwitchBox>
            </HelmInlineField>
            <HelmInlineField label='Scroll'>
                <SwitchBox>
                    <Switch value={alarmTablePaging.scroll} onChange={(val) => setAlarmTablePagingState('scroll', !alarmTablePaging.scroll)} />
                </SwitchBox>
            </HelmInlineField>
            <HelmInlineField label='Font size'>
                <Select
                    value={alarmTablePaging.fontSize}
                    options={fontSizeOptions}
                    onChange={(val) => setAlarmTablePagingState('fontSize', val)}
                />
            </HelmInlineField>
        </div>
    )
}
