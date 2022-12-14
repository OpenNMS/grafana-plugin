import { PanelProps } from '@grafana/data';
import { Button, ContextMenu, Modal, Pagination, Tab, TabContent, Table, TabsBar } from '@grafana/ui';

import React, { useState, useEffect } from 'react'
import { AlarmTableMenu } from './AlarmTableMenu';
import { AlarmTableModalContent } from './modal/AlarmTableModalContent';
import { AlarmTableSelectionStyles } from './AlarmTableSelectionStyles';
import { AlarmTableControlProps } from './AlarmTableTypes';
import { useAlarmHelmProperties } from './hooks/useAlarmHelmProperties';
import { useAlarmTableActions } from './hooks/useAlarmTableActions';
import { useAlarmTableConfigDefaults } from './hooks/useAlarmTableConfigDefaults';
import { useAlarmTableMenu } from './hooks/useAlarmTableMenu';
import { useAlarmTableRowHighlighter } from './hooks/useAlarmTableRowHighlighter';
import { useAlarmTableSelection } from './hooks/useAlarmTableSelection';
import { getDataSourceSrv } from '@grafana/runtime';
import { ClientDelegate } from 'lib/client_delegate';
import { OnmsAlarm } from 'opennms/src/model';


export const AlarmTableControl: React.FC<PanelProps<AlarmTableControlProps>> = (props) => {

    const [client, setClient] = useState<ClientDelegate>()
    const { state, rowClicked, soloIndex } = useAlarmTableSelection();
    const { table, menu, menuOpen, setMenuOpen } = useAlarmTableMenu(rowClicked);
    const { actions, detailsModal, setDetailsModal } = useAlarmTableActions(state.indexes, () => setMenuOpen(false));

    useAlarmTableRowHighlighter(state, table);
    useAlarmTableConfigDefaults(props.fieldConfig, props.onFieldConfigChange, props.options)
    const filteredProps = useAlarmHelmProperties(props?.data?.series[0], props?.options?.alarmTableData);
    const [tabActive, setTabActive] = useState(0);
    const tabClick = (e) => {
        setTabActive(e);
    }
    const alarmId = props.data?.series?.[0].fields.find((d) => d.name === 'ID')?.values.get(soloIndex)

    useEffect(() => {
        const rawDatasource = props.data?.request?.targets?.[0]?.datasource
        const updateDatasource = async () => {
            const datasources = getDataSourceSrv()
            const datasourceObject: any = await datasources.get(rawDatasource)
            setClient(datasourceObject.client);
        }
        if (rawDatasource) {
            updateDatasource();
        }
    }, [props?.data?.request?.targets])
    const [alarm, setAlarm] = useState<OnmsAlarm>();
    useEffect(() => {
        const updateAlarm = async () => {
            const returnedAlarm = await client?.getAlarm(alarmId);
            setAlarm(returnedAlarm)
        }
        if (alarmId !== alarm?.id){
            updateAlarm();
        }
    }, [alarm?.id, alarmId, client])
    const goToAlarm = () => {
        
        window.location.href = alarm?.detailsPage
    }
    return (
        <div ref={table}>
            <AlarmTableSelectionStyles />
            <Table data={filteredProps} width={props.width} height={props.height} />
            <Pagination numberOfPages={5} currentPage={1} onNavigate={() => { }} hideWhenSinglePage={true} />
            {menuOpen && <ContextMenu
                x={menu.x}
                y={menu.y}
                onClose={() => {
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
                    <AlarmTableModalContent tab={tabActive} alarmId={alarmId} client={client} />
                </TabContent>
            </Modal>
        </div>
    )
}
