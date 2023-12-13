import React, { useEffect, useState } from 'react'
import { PanelOptionsEditorProps } from '@grafana/data'
import { AlarmTableAdditional } from './AlarmTableAdditional'
import { AlarmTableAlarms } from './AlarmTableAlarms'
import { AlarmTableColumnSizes } from './AlarmTableColumnSizes'
import { AlarmTableData } from './AlarmTableData'
import { AlarmTablePaging } from './AlarmTablePaging'

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
      <AlarmTableAdditional context={context} onChange={(v) => onOptionChange(v, 'alarmTableAdditional')} />
      <AlarmTableData context={context} onChange={(v) => onOptionChange(v, 'alarmTableData')} />
      <AlarmTablePaging context={context} onChange={(v) => onOptionChange(v, 'alarmTablePaging')} />
      <AlarmTableAlarms onChange={(v) => onOptionChange(v, 'alarmTableAlarms')} alarmTable={context.options?.alarmTable} />
      <AlarmTableColumnSizes context={context} onChange={(v) => onOptionChange(v, 'alarmTableColumnSizes')} columnState={context.options?.alarmTable?.alarmTableColumnSizes} />
    </>
  )
}
