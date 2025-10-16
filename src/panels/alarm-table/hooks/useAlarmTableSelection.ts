import { useState } from 'react'
import { DataFrame } from '@grafana/data'
import { getAlarmIdsForRows } from '../AlarmTableHelper'
import { AlarmTableControlState } from '../AlarmTableTypes'

export const useAlarmTableSelection = (doubleClicked) => {

  const [alarmControlState, setAlarmControlState] = useState<AlarmTableControlState>(
    { selectedAlarmIds: new Set<number>(), selectedIndexes: [] as boolean[], lastClicked: -1, lastClickedAlarmId: -1 })
  const [soloAlarmId, setSoloAlarmId] = useState(-1)

  const toggleAlarmIdSelection = (id: number, alarmIds: Set<number>) => {
    if (alarmIds.has(id)) {
      alarmIds.delete(id)
    } else {
      alarmIds.add(id)
    }
  }

  const rowClicked = (alarmId: number, e: MouseEvent, rows: Element[], frame: DataFrame, fromContext = false) => {
    setAlarmControlState((previousState) => {
      let newSelectedAlarmIds: Set<number> = new Set(previousState.selectedAlarmIds)

      if ((!fromContext || newSelectedAlarmIds.size === 0) && (!e.shiftKey || previousState.lastClickedAlarmId < 1)) {
        // nothing was selected and user clicked or double clicked on a valid row
        if (!e.ctrlKey) {
          newSelectedAlarmIds = new Set<number>()
        }

        toggleAlarmIdSelection(alarmId, newSelectedAlarmIds)
        setSoloAlarmId(alarmId)

        if (e.detail === 2) {
          doubleClicked()
        }
      } else if (!fromContext) {
        // if some rows were already selected,
        // find the range of rows selected between previous selection and the
        // row user clicked on, and add those alarmIds to the selection list
        const rowAlarmIds = getAlarmIdsForRows(rows, frame)
        const currentIndex = rowAlarmIds.findIndex(i => i === alarmId) ?? -1
        const lastClickedIndex = rowAlarmIds.findIndex(i => i === previousState.lastClickedAlarmId) ?? -1

        let { start, end } = lastClickedIndex > currentIndex ?
            { start: currentIndex, end: lastClickedIndex >= 0 ? lastClickedIndex : currentIndex } :
            { start: lastClickedIndex >= 0 ? lastClickedIndex : currentIndex, end: currentIndex }

        for (let i = start; i <= end; i++) {
          newSelectedAlarmIds.add(rowAlarmIds[i])
        }

        setSoloAlarmId(0)
      } else if (fromContext && alarmId > 0) {
        const countSelected = previousState.selectedAlarmIds.size

        if (countSelected < 2) {
          newSelectedAlarmIds = new Set<number>()
          toggleAlarmIdSelection(alarmId, newSelectedAlarmIds)
          setSoloAlarmId(alarmId)
        }
      }

      return { selectedAlarmIds: newSelectedAlarmIds, selectedIndexes: [] as boolean[], lastClicked: -1, lastClickedAlarmId: alarmId }
    })
  }

  return { alarmControlState, setAlarmControlState, rowClicked, soloAlarmId }
}
