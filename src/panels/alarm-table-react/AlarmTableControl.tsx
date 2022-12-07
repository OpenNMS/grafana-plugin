import { css } from '@emotion/css'
import { GrafanaTheme2 } from '@grafana/data';
import { ContextMenu, Menu, MenuItem, Table, useStyles2 } from '@grafana/ui';

import React, { useRef, useState, useEffect } from 'react'

export const AlarmTableControl = (props) => {

    const getStyles = (theme: GrafanaTheme2) => {
        return {
            divider: css({
                height: 1,
                backgroundColor: theme.colors.border.weak,
                margin: theme.spacing(0.5, 0)
            })
        }
    }
    const styles = useStyles2(getStyles);
    const table = useRef<HTMLDivElement>(null);
    const [state, setState] = useState<{ indexes: boolean[], lastClicked: number }>({ indexes: [], lastClicked: -1 })
    const [menu, setMenu] = useState({ x: 0, y: 0 })
    const [menuOpen, setMenuOpen] = useState(false);

    const clearIndexes = (oldIndexes) => {
        const newIndexes = [...oldIndexes];
        return newIndexes.map(() => false)
    }

    const contextMenu = (index: number, e: MouseEvent) => {
        e.preventDefault();
        setMenu({ x: e.x, y: e.y })
        setMenuOpen(() => true);
        console.log('context! and setting menu')
    }

    const rowClicked = (index: number, e: MouseEvent) => {

        setState((updatedState) => {
            let newClickedIndexes = [...updatedState.indexes];
            if (!e.shiftKey || updatedState.lastClicked === -1) {
                if (!e.ctrlKey) {
                    newClickedIndexes = clearIndexes(newClickedIndexes);
                }
                newClickedIndexes[index] = newClickedIndexes[index] ? false : true;
            } else {
                let { start, end } = updatedState.lastClicked > index ?
                    { start: index, end: updatedState.lastClicked } :
                    { start: updatedState.lastClicked, end: index };
                for (let i = start; i <= end; i++) {
                    newClickedIndexes[i] = true;
                }
            }
            return { indexes: newClickedIndexes, lastClicked: index };
        });

    }

    useEffect(() => {
        const rows = table.current?.querySelectorAll('.table-body div[role="row"]')
        // if previous sibling is not clicked, or first item in entry, select-start
        // if previous sibling is clicked, and next sibling is clicked, select-continue
        // if previous sibling is clicked, and next sibling is not clicked, select-end
        if (rows) {
            rows.forEach((row, index) => {
                row.classList.remove('select-start')
                row.classList.remove('select-continue')
                row.classList.remove('select-end')
                if (!state.indexes[index - 1] && state.indexes[index]) {
                    row.classList.add('select-start')
                }
                if (state.indexes[index - 1] && state.indexes[index] && state.indexes[index + 1]) {
                    row.classList.add('select-continue')
                }
                if (state.indexes[index] && !state.indexes[index + 1]) {
                    row.classList.add('select-end')
                }
            })
        }
    }, [state.indexes])

    useEffect(() => {
        setTimeout(() => {
            const headerGroup = table.current?.querySelector('div[role="rowgroup"] + div')
            headerGroup?.classList.add('table-body')
            const rows = table.current?.querySelectorAll('.table-body div[role="row"]')
            rows?.forEach((row, index) => {
                row.addEventListener('click', (e: unknown) => rowClicked(index, e as MouseEvent))
                row.addEventListener('contextmenu', (e: unknown) => contextMenu(index, e as MouseEvent))
            })
        }, 50)
    }, [])
    const getMenu = () => {
        let items = [{ label: 'Details' }, { type: 'divider', label: '' }, { label: 'Acknowledge' }, { label: 'Escalate' }, { label: 'Clear' }]
        if (state.indexes.length > 1) {
            items = items.splice(2, items.length)
        }

        return (<Menu>
            {items.map((item, index) => {
                let elem = <MenuItem label={item.label} key={index} />
                if (item.type === 'divider') {
                    elem = <div className={styles.divider}></div>
                }

                return elem;
            })}
        </Menu>)
    }
    return (
        <div ref={table}>
            <style>
                {
                    `
                        div[role="row"] {
                            border:2px solid transparent;
                        }
                        div[role="cell"],div[role="row"] {
                            user-select:none;
                        }
                        div[role="row"].select-start {
                            border-top:2px dashed white;
                            border-right:2px dashed white;
                            border-left:2px dashed white;
                        }
                        div[role="row"].select-continue {
                            border-right:2px dashed white;
                            border-left:2px dashed white;
                        }
                         div[role="row"].select-end {
                            border-bottom:2px dashed white;
                            border-right:2px dashed white;
                            border-left:2px dashed white;
                        }
 
                    `
                }
            </style>
            <Table data={props?.data?.series?.[0]} width={props.width} height={props.height}>

            </Table>
            {menuOpen && <ContextMenu
                x={menu.x}
                y={menu.y}
                onClose={() => {
                    setMenuOpen(false);
                }}
                renderMenuItems={getMenu}
            />}
        </div>
    )
}
