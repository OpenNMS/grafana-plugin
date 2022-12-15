import { PanelProps } from '@grafana/data';
import { Button, ContextMenu, Modal, Pagination, Tab, TabContent, Table, TabsBar } from '@grafana/ui';

import React from 'react'
import { AlarmTableMenu } from './AlarmTableMenu';
import { AlarmTableModalContent } from './modal/AlarmTableModalContent';
import { AlarmTableSelectionStyles } from './AlarmTableSelectionStyles';
import { AlarmTableControlProps } from './AlarmTableTypes';
import { useAlarmHelmProperties } from './hooks/useAlarmHelmProperties';
import { useAlarmTableMenuActions } from './hooks/useAlarmTableMenuActions';
import { useAlarmTableConfigDefaults } from './hooks/useAlarmTableConfigDefaults';
import { useAlarmTableMenu } from './hooks/useAlarmTableMenu';
import { useAlarmTableRowHighlighter } from './hooks/useAlarmTableRowHighlighter';
import { useAlarmTableSelection } from './hooks/useAlarmTableSelection';
import { useAlarmTableModalTabs } from './hooks/useAlarmTableModalTabs';
import { useOpenNMSClient } from './hooks/useOpenNMSClient';
import { useAlarm } from './hooks/useAlarm';


export const AlarmTableControl: React.FC<PanelProps<AlarmTableControlProps>> = (props) => {


    const { state, rowClicked, soloIndex } = useAlarmTableSelection(() => {
        setDetailsModal(true)
    });

    const { table, menu, menuOpen, setMenuOpen } = useAlarmTableMenu(rowClicked);
    const { actions, detailsModal, setDetailsModal } = useAlarmTableMenuActions(state.indexes, () => setMenuOpen(false));
    const { tabActive, tabClick, resetTabs } = useAlarmTableModalTabs();
    const { client } = useOpenNMSClient(props.data?.request?.targets?.[0]?.datasource)
    const { alarm, goToAlarm, alarmQuery } = useAlarm(props?.data?.series, soloIndex, client);
    const { filteredProps, page, setPage, totalPages } = useAlarmHelmProperties(props?.data?.series[0], props?.options?.alarmTable);


    useAlarmTableRowHighlighter(state, table);
    useAlarmTableConfigDefaults(props.fieldConfig, props.onFieldConfigChange, props.options)
    const getFontSize = () => {
        const fontSize = props.options?.alarmTable.alarmTablePaging.fontSize?.value
        return `font-size-${fontSize}`
    }
    return (
        <div ref={table} className={
            `
            ${alarmQuery ? 'alarm-query' : 'non-alarm-query'}
            ${props.options?.alarmTable?.alarmTablePaging?.scroll ? 'scroll' : ' no-scroll'}
            ${getFontSize()}
            `
        }>
            <AlarmTableSelectionStyles />
            <div style={{ height: '90%' }}>
                {alarmQuery ? <Table data={filteredProps} width={props.width} height={props.height} /> :
                    <div>Select the Entity Datasource below, and choose an Alarm query to see results.</div>
                }
            </div>
            <div style={{ width: '100%', height: '10%' }}>
                <div>
                    <Pagination numberOfPages={totalPages === Infinity ? 0 : totalPages} currentPage={page} onNavigate={(b) => { setPage(b) }} hideWhenSinglePage={true} />
                </div>
            </div>
            {menuOpen && <ContextMenu
                x={menu.x}
                y={menu.y}
                onClose={() => {
                    resetTabs();
                    setMenuOpen(false);
                }}
                renderMenuItems={() => <AlarmTableMenu state={state} actions={actions} />}
            />}
            <Modal isOpen={detailsModal} title='Alarm Detail' onDismiss={() => setDetailsModal(false)}>
                <Button style={{ marginBottom: 12 }} onClick={goToAlarm}><i className='fa fa-external-link'></i>&nbsp;Full Details</Button>
                <TabsBar>
                    <Tab label='Overview' active={tabActive === 0} onChangeTab={() => tabClick(0)} />
                    <Tab label='Memos' active={tabActive === 1} onChangeTab={() => tabClick(1)} />
                    <Tab label='JSON' active={tabActive === 2} onChangeTab={() => tabClick(2)} />
                </TabsBar>
                <TabContent>
                    <AlarmTableModalContent tab={tabActive} alarm={alarm} client={client} />
                </TabContent>
            </Modal>
        </div>
    )
}
