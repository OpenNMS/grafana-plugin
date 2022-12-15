import { AlarmTableData } from "./AlarmTableData"
import React, { useEffect, useState } from 'react'
import { AlarmTablePaging } from "./AlarmTablePaging"
import { AlarmTableAlarms } from "./AlarmTableAlarms"
import { PanelOptionsEditorProps } from "@grafana/data"

export const AlarmTableOptions: React.FC<PanelOptionsEditorProps<{}>> = ({ context, onChange }) => {
    const [internalOptions, setInternalOptions] = useState({})
    const onOptionChange = (v, k) => {
        setInternalOptions((oldOptions) => {
            const newOptions = { ...oldOptions }
            newOptions[k] = v
            return newOptions
        })
    }

    useEffect(() => {
        onChange(internalOptions)
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [internalOptions])

    return (
        <>
            <AlarmTableData context={context} onChange={(v) => onOptionChange(v, 'alarmTableData')} />
            <AlarmTablePaging context={context} onChange={(v) => onOptionChange(v, 'alarmTablePaging')} />
            <AlarmTableAlarms onChange={(v) => onOptionChange(v, 'alarmTableAlarms')} alarmTable={context.options?.alarmTable} />
        </>
    )
}
