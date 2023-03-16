import { MappingType } from "@grafana/data"
import { helmColorArray } from "components/HelmColors"
import { useEffect } from 'react'
import { AlarmTableControlProps } from "../AlarmTableTypes"

export const useAlarmTableConfigDefaults = (fieldConfig, onFieldConfigChange, options: AlarmTableControlProps) => {
    const currentIndex = options?.alarmTable.alarmTableAlarms?.severityTheme?.value || 0

    useEffect(() => {
        const oldConfig = { ...fieldConfig }
        if (currentIndex !== 5) {
            oldConfig.defaults.mappings = [
                { type: MappingType.ValueToText, options: { INDETERMINATE: { color: helmColorArray[currentIndex][0], index: 0, text: 'Indeterminate' } } },
                { type: MappingType.ValueToText, options: { CLEARED: { color: helmColorArray[currentIndex][1], index: 1, text: 'Cleared' } } },
                { type: MappingType.ValueToText, options: { NORMAL: { color: helmColorArray[currentIndex][2], index: 2, text: 'Normal' } } },
                { type: MappingType.ValueToText, options: { WARNING: { color: helmColorArray[currentIndex][3], index: 3, text: 'Warning' } } },
                { type: MappingType.ValueToText, options: { MINOR: { color: helmColorArray[currentIndex][4], index: 4, text: 'Minor' } } },
                { type: MappingType.ValueToText, options: { MAJOR: { color: helmColorArray[currentIndex][5], index: 5, text: 'Major' } } },
                { type: MappingType.ValueToText, options: { CRITICAL: { color: helmColorArray[currentIndex][6], index: 6, text: 'Critical' } } },
            ]
            onFieldConfigChange(oldConfig)
        }
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [currentIndex])
}
