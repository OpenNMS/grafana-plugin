import { useState } from 'react'
import { AlarmTableControlState } from "../AlarmTableTypes";

export const useAlarmTableSelection = (doubleClicked) => {

    const [state, setState] = useState<AlarmTableControlState>({ indexes: [], lastClicked: -1 })
    const [soloIndex, setSoloIndex] = useState(-1);

    const clearIndexes = (oldIndexes) => {
        const newIndexes = [...oldIndexes];
        return newIndexes.map(() => false)
    }

    const allIndexesAreFalse = (indexes) => {
        const allFalse = !indexes.find((d) => d === true)
        return allFalse
    }
    const rowClicked = (index: number, e: MouseEvent, fromContext = false) => {
        setState((updatedState) => {
            let newClickedIndexes = [...updatedState.indexes];
            if ((!fromContext || allIndexesAreFalse(newClickedIndexes)) && (!e.shiftKey || updatedState.lastClicked === -1)) {
                if (!e.ctrlKey) {
                    newClickedIndexes = clearIndexes(newClickedIndexes);
                }
                newClickedIndexes[index] = newClickedIndexes[index] ? false : true;
                setSoloIndex(index);
                if (e.detail === 2) {
                    doubleClicked()
                }
            } else if (!fromContext) {
                let { start, end } = updatedState.lastClicked > index ?
                    { start: index, end: updatedState.lastClicked } :
                    { start: updatedState.lastClicked, end: index };
                for (let i = start; i <= end; i++) {
                    newClickedIndexes[i] = true;
                }
                setSoloIndex(-1);
            }
            return { indexes: newClickedIndexes, lastClicked: index };
        });
    }

    return { state, setState, clearIndexes, rowClicked, soloIndex }
}
