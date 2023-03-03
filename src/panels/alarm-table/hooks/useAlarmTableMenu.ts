import { useState, useEffect, useRef } from 'react'

export const useAlarmTableMenu = (rowClicked) => {

    const [menu, setMenu] = useState({ x: 0, y: 0 })
    const [menuOpen, setMenuOpen] = useState(false);

    const contextMenu = (index: number, e: MouseEvent) => {
        e.preventDefault();
        rowClicked(index, e,true);
        setMenu({ x: e.x, y: e.y })
        setMenuOpen(() => true);
    }

    const table = useRef<HTMLDivElement>(null);

    const addClassToTableBody = () => {
        const headerGroup = table.current?.querySelector('div[role="rowgroup"] + div')
        headerGroup?.classList.add('table-body')
    }

    useEffect(() => {
        addClassToTableBody();
        const rows = table.current?.querySelectorAll('.table-body div[role="row"]')
        rows?.forEach((row, index) => {
            row.removeEventListener('click', (e: unknown) => rowClicked(index, e as MouseEvent))
            row.removeEventListener('contextmenu', (e: unknown) => contextMenu(index, e as MouseEvent))
            row.addEventListener('click', (e: unknown) => rowClicked(index, e as MouseEvent))
            row.addEventListener('contextmenu', (e: unknown) => contextMenu(index, e as MouseEvent))
        })
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [table, menu, menuOpen, rowClicked])

    return { table, menu, menuOpen, setMenu, setMenuOpen }
}
