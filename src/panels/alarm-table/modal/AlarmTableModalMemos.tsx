import { config } from '@grafana/runtime'
import { Button, HorizontalGroup, TextArea } from '@grafana/ui';
import { ClientDelegate } from 'lib/client_delegate';
import { OnmsAlarm } from 'opennms/src/model'
import React, { useState, useEffect } from 'react'

interface AlarmTableModalMemosProps {
    alarm: OnmsAlarm | undefined;
    client: ClientDelegate | undefined;
}

export const AlarmTableModalMemos: React.FC<AlarmTableModalMemosProps> = ({ alarm, client }) => {
    const [localSticky, setLocalSticky] = useState(alarm?.sticky);
    const [localJournal, setLocalJournal] = useState(alarm?.journal);
    const [loading, setLoading] = useState(false);
    useEffect(() => {
        setLocalSticky(alarm?.sticky)
    }, [alarm?.sticky])
    useEffect(() => {
        setLocalJournal(alarm?.journal)
    }, [alarm?.journal])

    const stickyChange = (newSticky) => {
        setLocalSticky({ ...alarm?.sticky, body: newSticky.target.value })
    }

    const journalChange = (newJournal) => {
        setLocalJournal({ ...alarm?.journal, body: newJournal.target.value })
    }
    const saveSticky = async () => {
        setLoading(true);
        await client?.saveSticky(alarm?.id, localSticky?.body, config?.bootData?.user.id);
        setLoading(false);
    }
    const deleteSticky = async () => {
        setLoading(true);
        await client?.deleteSticky(alarm?.id)
        setLoading(false);
    }
    const saveJournal = async () => {
        setLoading(true);
        await client?.saveJournal(alarm?.id, localJournal?.body, config?.bootData?.user.id)
        setLoading(false);
    }
    const deleteJournal = async () => {
        setLoading(true);
        await client?.deleteJournal(alarm?.id)
        setLoading(false);
    }
    return (
        <div>
            <style>
                {
                    `
                    .sticky-memo {
                        display:flex;
                    }
                    .memo-header {
                        margin-top:24px;
                    }
                    .memo-wrapper {
                        margin-bottom:24px;
                    }
                    `
                }
            </style>
            <div className='memo-wrapper'>
                <h2 className='memo-header'>Sticky Memo</h2>
                <TextArea value={localSticky?.body} onChange={stickyChange}></TextArea>
                <HorizontalGroup className='sticky-memo'>
                    {loading}
                    <Button onClick={saveSticky}><i className="fa fa-plus"></i>&nbsp;Save</Button>
                    <Button variant='destructive' onClick={deleteSticky}><i className="fa fa-minus"></i>&nbsp;Delete</Button>
                </HorizontalGroup>
            </div>

            <div className='memo-wrapper'>
                <h2 className='memo-header'>Journal Memo</h2>
                <TextArea value={localJournal?.body} onChange={journalChange}></TextArea>
                <HorizontalGroup className='sticky-memo'>
                    {loading}
                    <Button onClick={saveJournal}><i className="fa fa-plus"></i>&nbsp;Save</Button>
                    <Button variant='destructive' onClick={deleteJournal}><i className="fa fa-minus"></i>&nbsp;Delete</Button>
                </HorizontalGroup>
            </div>

        </div>
    )
}
