import { MutableRefObject, RefObject, useEffect, useState } from 'react'
import { DataFrame } from '@grafana/data'
import { getAlarmIdFromCell } from '../AlarmTableHelper'

/**
 * 
 * @param indexes Row indexes and whether they are selected or not
 * @param selectedAlarmIds Set of selected alarm ids
 * @param rowClicked Row clicked handler
 * @param series Ref to the DataFrame of the alarm data
 * @param setAlarmControlState Set the AlarmTableControlState
 * @returns 
 */
export const useAlarmTableMenu = (
  table: RefObject<HTMLDivElement>,
  indexes: MutableRefObject<boolean[]>,
  selectedAlarmIds: MutableRefObject<Set<number>>,
  rowClicked,
  series: MutableRefObject<DataFrame>,
  setAlarmControlState
) => {

  const [menu, setMenu] = useState({ x: 0, y: 0 })
  const [menuOpen, setMenuOpen] = useState(false)

  const contextMenu = (alarmId: number, e: MouseEvent) => {
    e.preventDefault()

    if (alarmId > 0) {
      const rows = table.current?.querySelectorAll('.table-body div[role="row"]')
      rowClicked(alarmId, e, rows ? Array.from(rows) : [] as Element[], series.current, true)
    }

    setMenu({ x: e.x, y: e.y })
    setMenuOpen(() => true)
  }

  const addClassToTableBody = () => {
    const headerGroup = table.current?.querySelector('div[role="rowgroup"] + div')
    headerGroup?.classList.add('table-body')
  }

  // user made a regular click in the table
  const onTableClicked = (e: Event) => {
    const alarmId = getAlarmIdFromCell(e.target as HTMLElement, series.current)

    if (alarmId > 0) {
      // user clicked on a row, toggle selection
      const rows = table.current?.querySelectorAll('.table-body div[role="row"]')
      rowClicked(alarmId, e, rows ? Array.from(rows) : [] as Element[], series.current, false)
    } else {
      // user clicked on table background, clear all selections
      const newIndexes = indexes.current.map(x => false)
      setAlarmControlState({ selectedIndexes: newIndexes, selectedAlarmIds: new Set<number>(), lastClicked: -1, lastClickedAlarmId: alarmId })
    }
  }

  // User made a context-click (usually a right-click)
  const onTableContextMenu = (e: Event) => {
    const alarmId = getAlarmIdFromCell(e.target as HTMLElement, series.current)

    if (alarmId > 0) {
      // user context-clicked a row, toggle selection for that row, then launch context menu
      contextMenu(alarmId, e as MouseEvent)
    } else if (selectedAlarmIds.current.size > 0) {
      // user context-clicked but not in a row, but there are rows selected, so launch context menu
      contextMenu(0, e as MouseEvent)
    }
  }

  // apply click handlers to the table, but make sure they aren't applied to the Pagination component
  useEffect(() => {
    const currentTable = table.current
    const tableWrapper = currentTable?.querySelector('div.alarm-table-wrapper')

    tableWrapper?.addEventListener('click', onTableClicked)
    tableWrapper?.addEventListener('contextmenu', onTableContextMenu)

    return () => {
      tableWrapper?.removeEventListener('click', onTableClicked)
      tableWrapper?.removeEventListener('contextmenu', onTableContextMenu)
    }

    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [table.current])

  useEffect(() => {
    addClassToTableBody()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [menu, menuOpen, rowClicked, series.current])

  return { menu, menuOpen, setMenu, setMenuOpen }
}
