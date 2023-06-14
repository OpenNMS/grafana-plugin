import { useState } from 'react'
import { AlarmTableControlState } from '../AlarmTableTypes'

export const useAlarmTableSelection = (doubleClicked) => {

    const [state, setState] = useState<AlarmTableControlState>({ indexes: [], lastClicked: -1 })
    const [soloIndex, setSoloIndex] = useState(-1)

    const clearIndexes = (oldIndexes: boolean[]) => {
        return [...oldIndexes].map(() => false)
    }

    const allIndexesAreFalse = (indexes: boolean[]) => {
        return !indexes.some(i => i === true)
    }

    const rowClicked = (index: number, e: MouseEvent, fromContext = false) => {
        setState((previousState) => {
            let newClickedIndexes = [...previousState.indexes]

            if ((!fromContext || allIndexesAreFalse(newClickedIndexes)) && (!e.shiftKey || previousState.lastClicked === -1)) {
                if (!e.ctrlKey) {
                    newClickedIndexes = clearIndexes(newClickedIndexes)
                }

                newClickedIndexes[index] = newClickedIndexes[index] ? false : true
                setSoloIndex(index)

                if (e.detail === 2) {
                    doubleClicked()
                }
            } else if (!fromContext) {
                let { start, end } = previousState.lastClicked > index ?
                    { start: index, end: previousState.lastClicked } :
                    { start: previousState.lastClicked, end: index }

                for (let i = start; i <= end; i++) {
                    newClickedIndexes[i] = true
                }

                setSoloIndex(-1)
            } else if (fromContext && index >= 0) {
                const countSelected = previousState.indexes.filter(x => x === true).length

                if (countSelected < 2) {
                  newClickedIndexes = clearIndexes(newClickedIndexes)
                  newClickedIndexes[index] = true
                  setSoloIndex(index)
                }
            }

            return { indexes: newClickedIndexes, lastClicked: index }
        })
    }

    return { state, setState, clearIndexes, rowClicked, soloIndex }
}
