import { useState, useEffect, useRef } from 'react'

export const useAlarmTableMenu = (rowClicked) => {

    const [menu, setMenu] = useState({ x: 0, y: 0 })
    const [menuOpen, setMenuOpen] = useState(false)

    const contextMenu = (index: number, e: MouseEvent) => {
        e.preventDefault()
        rowClicked(index, e, true)
        setMenu({ x: e.x, y: e.y })
        setMenuOpen(() => true)
    }

    const table = useRef<HTMLDivElement>(null);

    const addClassToTableBody = () => {
        const headerGroup = table.current?.querySelector('div[role="rowgroup"] + div')
        headerGroup?.classList.add('table-body')
    }

    const processRowClicked = (e: Event) => {
        const index = (e.currentTarget as Element).getAttribute('data-rowindex')

        if (index !== null && index?.length > 0) {
          rowClicked(parseInt(index, 10), e as MouseEvent, false)
        }
    }

    const processContextMenu = (e: Event) => {
        const index = (e.currentTarget as Element).getAttribute('data-rowindex')

        if (index !== null && index?.length > 0) {
          contextMenu(parseInt(index, 10), e as MouseEvent)
        }
    }

    useEffect(() => {
        addClassToTableBody()

        const rows = table.current?.querySelectorAll('.table-body div[role="row"]')

        const onRowClicked = (e: Event) => processRowClicked(e)
        const onContextMenu = (e: Event) => processContextMenu(e)

        rows?.forEach((row, index) => {
            row.setAttribute('data-rowindex', '' + index)
            row.addEventListener('click', onRowClicked)
            row.addEventListener('contextmenu', onContextMenu)
        })

        return () => {
          if (rows && rows.length > 0) {
            rows.forEach((row) => {
              row.removeEventListener('click', onRowClicked)
              row.removeEventListener('contextmenu', onContextMenu)
            })
          }
        }
      // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [table, menu, menuOpen, rowClicked])

    return { table, menu, menuOpen, setMenu, setMenuOpen }
}
