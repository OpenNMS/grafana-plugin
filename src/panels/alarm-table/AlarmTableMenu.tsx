import { css } from '@emotion/css'
import { GrafanaTheme2 } from '@grafana/data'
import { Menu, MenuItem, useStyles2 } from '@grafana/ui'
import React from 'react'
import { AlarmTableControlActions, AlarmTableControlState } from './AlarmTableTypes'

interface AlarmTableMenuProps {
    state: AlarmTableControlState,
    actions: AlarmTableControlActions
}

export const AlarmTableMenu: React.FC<AlarmTableMenuProps> = ({ state,actions }) => {

    let items = [{
        label: 'Details',
        action: actions.details
    },
    { type: 'divider', label: '', },
    { label: 'Acknowledge', action: actions.acknowledge },
    { label: 'Escalate', action: actions.escalate },
    { label: 'Clear', action: actions.clear }
    ]

    // If we have more than one item selected
    // remove the ability to select Details
    if (state.indexes.filter((d) => d === true).length > 1) {
        items = items.splice(2, items.length)
    }
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

    return (
        <Menu>
            {items.map((item, index) => {
                let elem = <MenuItem label={item.label} key={index} onClick={item.action} />
              
                if (item.type === 'divider') {
                    elem = <div className={styles.divider}></div>
                }

                return elem;
            })}
        </Menu>
    )
}
