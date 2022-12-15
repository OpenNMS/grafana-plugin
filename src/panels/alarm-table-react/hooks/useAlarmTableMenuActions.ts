import { useState } from 'react'

export const useAlarmTableMenuActions = (indexes, closeMenu) => {
    const [detailsModal, setDetailsModal] = useState(false)
    const clear = () => {
        closeMenu();
    }

    const details = () => {
        closeMenu();
        setDetailsModal(true);
    }

    const escalate = () => {
        closeMenu();

    }

    const acknowledge = () => {
        closeMenu();

    }

    return { actions: { clear, details, escalate, acknowledge }, detailsModal,setDetailsModal }
}
